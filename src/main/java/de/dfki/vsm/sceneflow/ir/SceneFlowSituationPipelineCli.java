package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;

import java.nio.file.Path;

public final class SceneFlowSituationPipelineCli {

    public static void main(final String[] args) {
        if (args.length < 5) {
            System.err.println("Usage: SceneFlowSituationPipelineCli <snapshot.json> <sceneflow.xml> <output.xml> <report.json> <situation>");
            System.exit(2);
            return;
        }

        final Path snapshotPath = Path.of(args[0]);
        final Path sceneFlowPath = Path.of(args[1]);
        final Path outputPath = Path.of(args[2]);
        final Path reportPath = Path.of(args[3]);
        final String situation = args[4];
        final SceneFlowSituationPipeline.CandidateMode mode = SceneFlowSituationPipeline.CandidateMode
                .from(System.getProperty("sceneflow.pipeline.mode", "template"));
        final SceneFlowSituationPipeline.OutputMode outputMode = SceneFlowSituationPipeline.OutputMode
                .from(System.getProperty("sceneflow.pipeline.outputMode", "standalone"));
        final ConstraintResolutionMode constraintResolutionMode = ConstraintResolutionMode
                .from(System.getProperty("sceneflow.pipeline.constraintResolutionMode", "permissive"));
        final SceneFlowIrLlmCandidateProvider.Config llmConfig = new SceneFlowIrLlmCandidateProvider.Config(
                System.getProperty("sceneflow.llm.baseUrl", ""),
                System.getProperty("sceneflow.llm.apiKey", ""),
                System.getProperty("sceneflow.llm.model", ""),
                parseInt(System.getProperty("sceneflow.llm.timeoutSec", "30"), 30),
                parseInt(System.getProperty("sceneflow.llm.maxCandidates", "3"), 3)
        );
        final Path generatedProjectPath = Path.of(System.getProperty(
                "sceneflow.pipeline.projectOutDir",
                outputPath.toAbsolutePath().getParent() == null
                        ? "build/reports/sceneflow-generated-project"
                        : outputPath.toAbsolutePath().getParent().resolve("sceneflow-generated-project").toString()
        ));

        try {
            final JSONObject report = new SceneFlowSituationPipeline().run(
                    snapshotPath,
                    sceneFlowPath,
                    outputPath,
                    reportPath,
                    situation,
                    new SceneFlowSituationPipeline.Settings(mode, outputMode, llmConfig, constraintResolutionMode),
                    generatedProjectPath);
            if ("success".equals(report.optString("status", ""))) {
                System.out.println("OK: generated flow written to " + outputPath.toAbsolutePath());
                if (!report.isNull("generatedProjectPath")) {
                    System.out.println("OK: generated VSM project written to " + report.optString("generatedProjectPath"));
                }
                System.out.println("OK: report written to " + reportPath.toAbsolutePath());
                return;
            }
            System.err.println("FAILED: no candidate passed validation/compile. See report: " + reportPath.toAbsolutePath());
            System.exit(1);
        } catch (SceneFlowIrCompileException exc) {
            System.err.println("FAILED: " + exc.getMessage());
            System.exit(1);
        }
    }

    private static int parseInt(final String value, final int fallback) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException ignored) {
            return fallback;
        }
    }
}
