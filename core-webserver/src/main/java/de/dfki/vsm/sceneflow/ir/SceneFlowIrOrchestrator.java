package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public final class SceneFlowIrOrchestrator {

    private final SceneFlowIrSemanticValidator semanticValidator;
    private final SceneFlowIrCompiler compiler;

    public SceneFlowIrOrchestrator() {
        this.semanticValidator = new SceneFlowIrSemanticValidator();
        this.compiler = new SceneFlowIrCompiler();
    }

    public SceneFlowGenerationResult generateFlowFromSituation(
            final Path snapshotPath,
            final Path baseSceneFlowPath,
            final Path outputPath,
            final List<Path> irCandidates) throws SceneFlowIrCompileException {
        if (irCandidates == null || irCandidates.isEmpty()) {
            throw new SceneFlowIrCompileException("No IR candidates provided.");
        }

        final JSONObject snapshot = readJson(snapshotPath);
        final SceneFlow baseFlow = loadSceneFlow(baseSceneFlowPath);
        final List<SceneFlowGenerationAttempt> attempts = new ArrayList<>();

        int attemptNo = 1;
        for (Path irPath : irCandidates) {
            final JSONObject ir = readJson(irPath);
            final SemanticValidationResult semanticResult = semanticValidator.validate(ir, snapshot);
            if (semanticResult.hasErrors()) {
                attempts.add(new SceneFlowGenerationAttempt(
                        attemptNo++,
                        irPath.toString(),
                        semanticResult.getIssues(),
                        null,
                        false));
                continue;
            }

            try {
                final SceneFlow compiled = compiler.compilePatch(ir, baseFlow);
                if (!XMLUtilities.writeToXMLFile(compiled, outputPath.toFile(), "UTF-8")) {
                    throw new SceneFlowIrCompileException("Cannot write compiled SceneFlow XML to " + outputPath);
                }
                attempts.add(new SceneFlowGenerationAttempt(
                        attemptNo,
                        irPath.toString(),
                        List.of(),
                        null,
                        true));
                return new SceneFlowGenerationResult(true, outputPath, attempts);
            } catch (SceneFlowIrCompileException exc) {
                attempts.add(new SceneFlowGenerationAttempt(
                        attemptNo,
                        irPath.toString(),
                        List.of(),
                        exc.getMessage(),
                        false));
                attemptNo++;
            }
        }

        return new SceneFlowGenerationResult(false, outputPath, attempts);
    }

    private JSONObject readJson(final Path path) throws SceneFlowIrCompileException {
        try (var reader = Files.newBufferedReader(path)) {
            return new JSONObject(new JSONTokener(reader));
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Cannot read JSON file: " + path, exc);
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

