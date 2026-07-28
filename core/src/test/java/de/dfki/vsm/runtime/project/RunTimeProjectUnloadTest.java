package de.dfki.vsm.runtime.project;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Regression test for a leak found via a real concurrent load/soak test (Phase 6,
 * doc/vsm-workspace-platform-plan.md): a client that calls unload() directly on a still-running
 * project (e.g. closing it without Stopping first — a perfectly ordinary WebUiServer.handleUnload
 * call sequence) used to leave the Interpreter's SceneFlow thread running forever, since
 * unload() only nulled the mInterpreter reference instead of aborting it. Over repeated
 * open/close cycles this accumulated one live thread per cycle.
 */
class RunTimeProjectUnloadTest {

    private static final String PROJECT_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            + "<Project name=\"UnloadTest\" androidProject=\"false\" uuid=\"11111111-0000-0000-0000-000000000001\">\n"
            + "  <Plugins>\n  </Plugins>\n  <Agents>\n  </Agents>\n  <Player>\n  </Player>\n</Project>\n";

    // A self-looping TEdge, same shape as benchmark/minimal's fixture — guarantees the
    // interpreter has a live SceneFlow thread to leak if unload() doesn't stop it.
    private static final String SCENEFLOW_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            + "<SceneFlow id=\"\" name=\"UnloadTest\" hideLocalVar=\"false\" hideGlobalVar=\"false\" "
            + "start=\"N1;\" context=\"\" package=\"\" xmlns=\"xml.sceneflow.dfki.de\" "
            + "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
            + "xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n"
            + "  <Define>\n  </Define>\n  <Declare>\n  </Declare>\n  <Commands>\n  </Commands>\n"
            + "  <Node id=\"N1\" name=\"Loop\" history=\"false\">\n"
            + "    <Define>\n    </Define>\n    <Declare>\n    </Declare>\n    <Commands>\n    </Commands>\n"
            + "    <TEdge target=\"N1\" start=\"\" timeout=\"100\">\n"
            + "      <Graphics>\n        <Connection>\n"
            + "          <ControlPoint xPos=\"0\" yPos=\"0\" ctrlXPos=\"0\" ctrlYPos=\"0\"/>\n"
            + "          <ControlPoint xPos=\"0\" yPos=\"0\" ctrlXPos=\"0\" ctrlYPos=\"0\"/>\n"
            + "        </Connection>\n      </Graphics>\n    </TEdge>\n"
            + "    <Graphics>\n      <Position xPos=\"0\" yPos=\"0\"/>\n    </Graphics>\n"
            + "  </Node>\n  <ClassPath>\n  </ClassPath>\n  <InitContext></InitContext>\n</SceneFlow>\n";

    private void writeMinimalProject(Path dir) throws IOException {
        Files.writeString(dir.resolve("project.xml"), PROJECT_XML);
        Files.writeString(dir.resolve("sceneflow.xml"), SCENEFLOW_XML);
        Files.writeString(dir.resolve("scenescript.xml"),
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SceneScript lower=\"0\" upper=\"0\">\n</SceneScript>\n");
        Files.writeString(dir.resolve("acticon.xml"),
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Acticon>\n</Acticon>\n");
        Files.writeString(dir.resolve("visicon.xml"),
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Visicon>\n</Visicon>\n");
        Files.writeString(dir.resolve("gesticon.xml"),
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Gesticon>\n</Gesticon>\n");
    }

    private static Set<Thread> snapshotThreads() {
        return new HashSet<>(Thread.getAllStackTraces().keySet());
    }

    @Test
    void unloadStopsTheSceneFlowThreadEvenWithoutAPriorAbort(@TempDir Path tempDir) throws Exception {
        writeMinimalProject(tempDir);

        RunTimeProject project = new RunTimeProject();
        assertTrue(project.parse(tempDir.toString()), "minimal fixture project must parse");

        Set<Thread> before = snapshotThreads();
        assertTrue(project.launch());
        assertTrue(project.start());

        // Wait for the interpreter's own thread(s) to actually appear.
        Set<Thread> spawned = new HashSet<>();
        long deadline = System.currentTimeMillis() + 5000;
        while (spawned.isEmpty() && System.currentTimeMillis() < deadline) {
            Set<Thread> after = snapshotThreads();
            after.removeAll(before);
            after.removeIf(t -> !t.isAlive());
            spawned = after;
            if (spawned.isEmpty()) {
                Thread.sleep(50);
            }
        }
        assertFalse(spawned.isEmpty(), "starting the project must spawn at least one live thread");

        // The regression: call unload() directly, the way WebUiServer.handleUnload does for a
        // still-running project, WITHOUT calling abort()/Stop first.
        assertTrue(project.unload());

        deadline = System.currentTimeMillis() + 5000;
        Set<Thread> stillAlive;
        do {
            stillAlive = new HashSet<>(spawned);
            stillAlive.removeIf(t -> !t.isAlive());
            if (!stillAlive.isEmpty()) {
                Thread.sleep(50);
            }
        } while (!stillAlive.isEmpty() && System.currentTimeMillis() < deadline);

        assertTrue(stillAlive.isEmpty(),
                "unload() must stop every thread the interpreter spawned, even without a prior "
                + "abort()/Stop — still alive: " + stillAlive);
    }
}
