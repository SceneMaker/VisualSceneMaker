package de.dfki.vsm.sceneflow.ir;

import java.nio.file.Path;

public final class SceneFlowIrCompilerCli {

    public static void main(final String[] args) {
        if (args.length < 2 || args.length > 3) {
            System.err.println("Usage: SceneFlowIrCompilerCli <ir.json> <sceneflow.xml> [output.xml]");
            System.exit(2);
            return;
        }

        final Path irPath = Path.of(args[0]);
        final Path sceneFlowPath = Path.of(args[1]);
        final Path outputPath = args.length >= 3 ? Path.of(args[2]) : sceneFlowPath;

        try {
            new SceneFlowIrCompiler().compileToFile(irPath, sceneFlowPath, outputPath);
            System.out.println("OK: compiled SceneFlow XML written to " + outputPath.toAbsolutePath());
        } catch (SceneFlowIrCompileException exc) {
            System.err.println("FAILED: " + exc.getMessage());
            System.exit(1);
        }
    }
}

