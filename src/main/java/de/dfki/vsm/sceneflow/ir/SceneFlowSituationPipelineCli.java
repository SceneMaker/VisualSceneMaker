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

        try {
            final JSONObject report = new SceneFlowSituationPipeline().run(
                    snapshotPath, sceneFlowPath, outputPath, reportPath, situation);
            if ("success".equals(report.optString("status", ""))) {
                System.out.println("OK: generated flow written to " + outputPath.toAbsolutePath());
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
}

