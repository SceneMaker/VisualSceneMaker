package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public final class SceneFlowNarrativeExplainerCli {

    public static void main(final String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: SceneFlowNarrativeExplainerCli <sceneflow.xml> <out.json>");
            System.exit(2);
            return;
        }

        final Path sceneFlowPath = Path.of(args[0]);
        final Path outPath = Path.of(args[1]);
        try {
            final JSONObject report = new SceneFlowNarrativeExplainer().explain(sceneFlowPath);
            if (outPath.getParent() != null) {
                Files.createDirectories(outPath.getParent());
            }
            Files.writeString(outPath, report.toString(2), StandardCharsets.UTF_8);
            System.out.println("OK: explanation written to " + outPath.toAbsolutePath());
        } catch (SceneFlowIrCompileException | IOException exc) {
            System.err.println("FAILED: " + exc.getMessage());
            System.exit(1);
        }
    }
}
