package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public final class SceneFlowSituationPipeline {

    public enum CandidateMode {
        TEMPLATE,
        LLM,
        HYBRID;

        public static CandidateMode from(final String value) {
            if (value == null || value.isBlank()) {
                return TEMPLATE;
            }
            switch (value.trim().toLowerCase(Locale.ROOT)) {
                case "template":
                    return TEMPLATE;
                case "llm":
                    return LLM;
                case "hybrid":
                    return HYBRID;
                default:
                    return TEMPLATE;
            }
        }
    }

    public static final class Settings {
        private final CandidateMode mode;
        private final SceneFlowIrLlmCandidateProvider.Config llm;

        public Settings(final CandidateMode mode, final SceneFlowIrLlmCandidateProvider.Config llm) {
            this.mode = mode == null ? CandidateMode.TEMPLATE : mode;
            this.llm = llm;
        }

        public CandidateMode mode() {
            return mode;
        }

        public SceneFlowIrLlmCandidateProvider.Config llm() {
            return llm;
        }
    }

    public JSONObject run(
            final Path snapshotPath,
            final Path baseSceneFlowPath,
            final Path outputPath,
            final Path reportPath,
            final String situation) throws SceneFlowIrCompileException {
        return run(snapshotPath, baseSceneFlowPath, outputPath, reportPath, situation,
                new Settings(CandidateMode.TEMPLATE, null));
    }

    public JSONObject run(
            final Path snapshotPath,
            final Path baseSceneFlowPath,
            final Path outputPath,
            final Path reportPath,
            final String situation,
            final Settings settings) throws SceneFlowIrCompileException {
        final JSONObject snapshot = readJson(snapshotPath);
        final SceneFlow baseFlow = loadSceneFlow(baseSceneFlowPath);
        final Settings effectiveSettings = settings == null
                ? new Settings(CandidateMode.TEMPLATE, null)
                : settings;
        final List<String> generationWarnings = new ArrayList<>();
        final List<JSONObject> candidates = generateCandidates(
                situation, snapshot, effectiveSettings, generationWarnings);

        final SceneFlowIrSemanticValidator semanticValidator = new SceneFlowIrSemanticValidator();
        final SceneFlowIrCompiler compiler = new SceneFlowIrCompiler();

        final JSONArray attempts = new JSONArray();
        JSONObject chosen = null;
        int successAttempt = -1;

        for (int i = 0; i < candidates.size(); i++) {
            final JSONObject candidate = candidates.get(i);
            final JSONObject attempt = new JSONObject();
            final String source = candidate.optJSONObject("metadata") != null
                    ? candidate.optJSONObject("metadata").optString("source", "unknown")
                    : "unknown";
            attempt.put("attempt", i + 1);
            attempt.put("templateSource", source);

            final SemanticValidationResult semantic = semanticValidator.validate(candidate, snapshot);
            if (semantic.hasErrors()) {
                attempt.put("status", "semantic_rejected");
                final JSONArray issues = new JSONArray();
                for (SemanticIssue issue : semantic.getIssues()) {
                    issues.put(new JSONObject()
                            .put("code", issue.getCode())
                            .put("path", issue.getPath())
                            .put("message", issue.getMessage()));
                }
                attempt.put("semanticIssues", issues);
                attempts.put(attempt);
                continue;
            }

            try {
                final SceneFlow compiled = compiler.compilePatch(candidate, baseFlow);
                if (!XMLUtilities.writeToXMLFile(compiled, outputPath.toFile(), "UTF-8")) {
                    throw new SceneFlowIrCompileException("Cannot write compiled SceneFlow XML to " + outputPath);
                }
                attempt.put("status", "accepted");
                attempts.put(attempt);
                chosen = candidate;
                successAttempt = i + 1;
                break;
            } catch (SceneFlowIrCompileException exc) {
                attempt.put("status", "compile_rejected");
                attempt.put("compileError", exc.getMessage());
                attempts.put(attempt);
            }
        }

        final JSONObject report = new JSONObject()
                .put("pipelineVersion", "1.0")
                .put("generatedAt", Instant.now().toString())
                .put("situation", situation == null ? "" : situation)
                .put("snapshotPath", snapshotPath.toAbsolutePath().toString())
                .put("sceneFlowPath", baseSceneFlowPath.toAbsolutePath().toString())
                .put("outputPath", outputPath.toAbsolutePath().toString())
                .put("candidateMode", effectiveSettings.mode().name().toLowerCase(Locale.ROOT))
                .put("attemptCount", attempts.length())
                .put("attempts", attempts)
                .put("generationWarnings", new JSONArray(generationWarnings));

        if (chosen != null) {
            final JSONObject metadata = chosen.optJSONObject("metadata");
            report.put("status", "success");
            report.put("successAttempt", successAttempt);
            report.put("chosenTemplate", metadata == null ? "unknown" : metadata.optString("source", "unknown"));
            report.put("assumptions", chosen.optJSONArray("assumptions") == null
                    ? new JSONArray()
                    : chosen.optJSONArray("assumptions"));
        } else {
            report.put("status", "failed");
            report.put("successAttempt", JSONObject.NULL);
            report.put("chosenTemplate", JSONObject.NULL);
            report.put("assumptions", new JSONArray());
        }

        writeJson(reportPath, report);
        return report;
    }

    private List<JSONObject> generateCandidates(
            final String situation,
            final JSONObject snapshot,
            final Settings settings,
            final List<String> warnings) throws SceneFlowIrCompileException {
        final SceneFlowIrTemplateLibrary templateLibrary = new SceneFlowIrTemplateLibrary();
        final SceneFlowIrLlmCandidateProvider llmProvider = new SceneFlowIrLlmCandidateProvider();

        switch (settings.mode()) {
            case TEMPLATE:
                return templateLibrary.generateCandidates(situation, snapshot);
            case LLM:
                return llmProvider.generateCandidates(situation, snapshot, settings.llm());
            case HYBRID:
                final List<JSONObject> merged = new ArrayList<>();
                try {
                    merged.addAll(llmProvider.generateCandidates(situation, snapshot, settings.llm()));
                } catch (SceneFlowIrCompileException exc) {
                    warnings.add("LLM generation unavailable, falling back to template candidates: " + exc.getMessage());
                }
                merged.addAll(templateLibrary.generateCandidates(situation, snapshot));
                return merged;
            default:
                return templateLibrary.generateCandidates(situation, snapshot);
        }
    }

    private JSONObject readJson(final Path path) throws SceneFlowIrCompileException {
        try (var reader = Files.newBufferedReader(path)) {
            return new JSONObject(new JSONTokener(reader));
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Cannot read JSON file: " + path, exc);
        }
    }

    private void writeJson(final Path path, final JSONObject json) throws SceneFlowIrCompileException {
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            Files.writeString(path, json.toString(2) + System.lineSeparator());
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Cannot write report JSON file: " + path, exc);
        }
    }

    private SceneFlow loadSceneFlow(final Path sceneFlowXmlPath) throws SceneFlowIrCompileException {
        final SceneFlow sceneFlow = new SceneFlow();
        if (!XMLUtilities.parseFromXMLFile(sceneFlow, sceneFlowXmlPath.toFile())) {
            throw new SceneFlowIrCompileException("Cannot parse SceneFlow XML file: " + sceneFlowXmlPath);
        }
        sceneFlow.establishStartNodes();
        sceneFlow.establishTargetNodes();
        sceneFlow.establishAltStartNodes();
        return sceneFlow;
    }
}
