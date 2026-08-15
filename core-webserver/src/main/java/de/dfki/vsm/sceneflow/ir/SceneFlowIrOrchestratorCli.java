package de.dfki.vsm.sceneflow.ir;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public final class SceneFlowIrOrchestratorCli {

    public static void main(final String[] args) {
        if (args.length < 4) {
            System.err.println("Usage: SceneFlowIrOrchestratorCli <snapshot.json> <sceneflow.xml> <output.xml> <ir1.json> [ir2.json ...]");
            System.exit(2);
            return;
        }

        final Path snapshotPath = Path.of(args[0]);
        final Path sceneFlowPath = Path.of(args[1]);
        final Path outputPath = Path.of(args[2]);
        final List<Path> irCandidates = new ArrayList<>();
        for (int i = 3; i < args.length; i++) {
            irCandidates.add(Path.of(args[i]));
        }

        try {
            final SceneFlowGenerationResult result = new SceneFlowIrOrchestrator().generateFlowFromSituation(
                    snapshotPath, sceneFlowPath, outputPath, irCandidates);

            for (SceneFlowGenerationAttempt attempt : result.getAttempts()) {
                System.out.println("attempt=" + attempt.getAttempt() + " ir=" + attempt.getIrSource() + " success=" + attempt.isSuccess());
                for (SemanticIssue issue : attempt.getSemanticIssues()) {
                    System.out.println("  semantic " + issue.getCode() + " " + issue.getPath() + " :: " + issue.getMessage());
                }
                if (attempt.getCompileError() != null && !attempt.getCompileError().isBlank()) {
                    System.out.println("  compile " + attempt.getCompileError());
                }
            }

            if (result.isSuccess()) {
                System.out.println("OK: generated SceneFlow written to " + result.getOutputPath().toAbsolutePath());
                return;
            }

            System.err.println("FAILED: all IR candidates were rejected.");
            System.exit(1);
        } catch (SceneFlowIrCompileException exc) {
            System.err.println("FAILED: " + exc.getMessage());
            System.exit(1);
        }
    }
}

