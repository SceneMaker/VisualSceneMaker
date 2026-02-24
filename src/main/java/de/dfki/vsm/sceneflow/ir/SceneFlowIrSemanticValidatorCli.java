package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public final class SceneFlowIrSemanticValidatorCli {

    public static void main(final String[] args) throws IOException {
        if (args.length < 2) {
            System.err.println("Usage: SceneFlowIrSemanticValidatorCli <ir.json> <snapshot.json>");
            System.exit(2);
            return;
        }

        final Path irPath = Path.of(args[0]);
        final Path snapshotPath = Path.of(args[1]);
        final JSONObject ir = readJson(irPath);
        final JSONObject snapshot = readJson(snapshotPath);

        final SceneFlowIrSemanticValidator validator = new SceneFlowIrSemanticValidator();
        final SemanticValidationResult result = validator.validate(ir, snapshot);

        if (!result.hasErrors()) {
            if (result.hasIssues()) {
                System.out.println("OK: semantic validation passed with warnings (" + result.getIssues().size() + ")");
                for (SemanticIssue issue : result.getIssues()) {
                    System.out.println(issue.getSeverity() + " " + issue.getCode() + " "
                            + issue.getPath() + " :: " + issue.getMessage());
                }
            } else {
                System.out.println("OK: semantic validation passed");
            }
            return;
        }

        System.err.println("FAILED: semantic validation found " + result.getIssues().size() + " issue(s)");
        for (SemanticIssue issue : result.getIssues()) {
            System.err.println(issue.getSeverity() + " " + issue.getCode() + " "
                    + issue.getPath() + " :: " + issue.getMessage());
        }
        System.exit(1);
    }

    private static JSONObject readJson(final Path path) throws IOException {
        try (var reader = Files.newBufferedReader(path)) {
            return new JSONObject(new JSONTokener(reader));
        }
    }
}
