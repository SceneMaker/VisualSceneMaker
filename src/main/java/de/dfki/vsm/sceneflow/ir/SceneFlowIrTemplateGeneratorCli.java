package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public final class SceneFlowIrTemplateGeneratorCli {

    public static void main(final String[] args) throws IOException {
        if (args.length < 3) {
            System.err.println("Usage: SceneFlowIrTemplateGeneratorCli <snapshot.json> <out-dir> <situation>");
            System.exit(2);
            return;
        }

        final Path snapshotPath = Path.of(args[0]);
        final Path outDir = Path.of(args[1]);
        final String situation = args[2];

        final JSONObject snapshot;
        try (var reader = Files.newBufferedReader(snapshotPath)) {
            snapshot = new JSONObject(new JSONTokener(reader));
        }

        final List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(situation, snapshot);
        Files.createDirectories(outDir);
        for (int i = 0; i < candidates.size(); i++) {
            final Path out = outDir.resolve("candidate-" + (i + 1) + ".json");
            Files.writeString(out, candidates.get(i).toString(2) + System.lineSeparator());
            System.out.println("Wrote " + out.toAbsolutePath());
        }
    }
}

