package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public final class SceneFlowIrCompiler {

    public SceneFlow compilePatch(final JSONObject ir, final SceneFlow baseFlow) throws SceneFlowIrCompileException {
        final SceneFlow compiled = new SceneFlowIrPatchApplier().apply(ir, baseFlow);
        compiled.establishStartNodes();
        compiled.establishTargetNodes();
        compiled.establishAltStartNodes();
        return compiled;
    }

    public SceneFlow compileFromFiles(final Path irPath, final Path sceneFlowXmlPath) throws SceneFlowIrCompileException {
        final JSONObject ir = readJson(irPath);
        final SceneFlow base = loadSceneFlow(sceneFlowXmlPath);
        return compilePatch(ir, base);
    }

    public void compileToFile(final Path irPath, final Path sceneFlowXmlPath, final Path outputPath)
            throws SceneFlowIrCompileException {
        final SceneFlow compiled = compileFromFiles(irPath, sceneFlowXmlPath);
        if (!XMLUtilities.writeToXMLFile(compiled, outputPath.toFile(), "UTF-8")) {
            throw new SceneFlowIrCompileException("Cannot write compiled SceneFlow XML to " + outputPath);
        }
    }

    private SceneFlow loadSceneFlow(final Path sceneFlowXmlPath) throws SceneFlowIrCompileException {
        final SceneFlow sceneFlow = new SceneFlow();
        final File file = sceneFlowXmlPath.toFile();
        if (!XMLUtilities.parseFromXMLFile(sceneFlow, file)) {
            throw new SceneFlowIrCompileException("Cannot parse SceneFlow XML file: " + sceneFlowXmlPath);
        }
        sceneFlow.establishStartNodes();
        sceneFlow.establishTargetNodes();
        sceneFlow.establishAltStartNodes();
        return sceneFlow;
    }

    private JSONObject readJson(final Path path) throws SceneFlowIrCompileException {
        try (var reader = Files.newBufferedReader(path)) {
            return new JSONObject(new JSONTokener(reader));
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Cannot read JSON file: " + path, exc);
        }
    }
}

