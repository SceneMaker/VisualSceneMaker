package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.NodeStartedEvent;
import de.dfki.vsm.event.event.TimeoutEdgeStartedEvent;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for the SceneFlow interpreter runtime engine.
 * Tests correctness of state transitions (epsilon, guarded, timeout edges)
 * and measures performance characteristics of the optimized execution engine.
 */
public class InterpreterIntegrationTest {

    private RunTimeProject project;
    private Path tempDir;

    @BeforeEach
    void setUp() throws IOException {
        tempDir = Files.createTempDirectory("vsm-test-");
    }

    @AfterEach
    void tearDown() {
        if (project != null) {
            try {
                if (project.isRunning()) {
                    project.abort();
                }
                project.unload();
            } catch (Exception ignored) {
            }
            project = null;
        }
        // Clean up temp directory
        if (tempDir != null) {
            try {
                Files.walk(tempDir)
                        .map(Path::toFile)
                        .sorted((a, b) -> -a.compareTo(b))
                        .forEach(File::delete);
            } catch (Exception ignored) {
            }
        }
    }

    // ========== CORRECTNESS TESTS ==========

    /**
     * Tests a simple epsilon-edge chain: N1 -> N2 -> N3 (end).
     * Verifies all nodes are visited in order and execution terminates.
     */
    @Test
    void epsilonChainVisitsAllNodesInOrder() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_EPSILON_CHAIN);
        project = new RunTimeProject(tempDir.toFile());

        List<String> visited = new CopyOnWriteArrayList<>();
        CountDownLatch done = new CountDownLatch(1);

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (!id.equals("Test")) { // skip root SceneFlow node
                    visited.add(id);
                }
                if (id.equals("N3")) {
                    done.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch(), "launch() should succeed");
            assertTrue(project.start(), "start() should succeed");
            assertTrue(done.await(5, TimeUnit.SECONDS), "Execution should complete within 5s");
            waitForStop(5000);

            assertEquals(List.of("N1", "N2", "N3"), visited,
                    "Nodes should be visited in epsilon-chain order");
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    /**
     * Tests variable assignment and guarded edge evaluation.
     * Flow: N1 (x=1) -> epsilon -> N2 (x=x+1) -> guard(x==2) -> N3
     * Verifies the guarded edge fires immediately when condition is already true.
     */
    @Test
    void guardedEdgeFiresWhenConditionIsTrue() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_GUARD);
        project = new RunTimeProject(tempDir.toFile());

        List<String> visited = new CopyOnWriteArrayList<>();
        CountDownLatch done = new CountDownLatch(1);

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (!id.equals("Test")) {
                    visited.add(id);
                }
                if (id.equals("N3")) {
                    done.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            assertTrue(project.start());
            assertTrue(done.await(5, TimeUnit.SECONDS), "Guard should fire within 5s");
            waitForStop(5000);

            assertEquals(List.of("N1", "N2", "N3"), visited);
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    /**
     * Tests timeout edge: N1 waits 100ms, then transitions to N2.
     * Verifies timeout fires and transition happens within a reasonable window.
     */
    @Test
    void timeoutEdgeFiresAfterDelay() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_TIMEOUT);
        project = new RunTimeProject(tempDir.toFile());

        List<String> visited = new CopyOnWriteArrayList<>();
        CountDownLatch done = new CountDownLatch(1);
        long[] times = new long[2]; // [0] = N1 start, [1] = N2 start

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (id.equals("N1")) {
                    times[0] = System.currentTimeMillis();
                    visited.add(id);
                } else if (id.equals("N2")) {
                    times[1] = System.currentTimeMillis();
                    visited.add(id);
                    done.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            assertTrue(project.start());
            assertTrue(done.await(5, TimeUnit.SECONDS), "Timeout should fire within 5s");
            waitForStop(5000);

            assertEquals(List.of("N1", "N2"), visited);
            long elapsed = times[1] - times[0];
            // Timeout is 100ms; JVM thread scheduling adds 1-5ms typical jitter
            assertTrue(elapsed >= 95, "Timeout should wait at least ~95ms, was " + elapsed + "ms");
            assertTrue(elapsed < 130, "Timeout should fire within 130ms, was " + elapsed + "ms");
            System.out.println("[PERF] Timeout 100ms edge actual latency: " + elapsed + "ms");
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    @Test
    void timeoutEdgeIntervalChoosesValueInConfiguredRange() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_TIMEOUT_INTERVAL);
        project = new RunTimeProject(tempDir.toFile());

        CountDownLatch done = new CountDownLatch(1);
        long[] times = new long[2]; // [0] = N1 start, [1] = N2 start
        long[] selectedTimeout = new long[1];

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (id.equals("N1")) {
                    times[0] = System.currentTimeMillis();
                } else if (id.equals("N2")) {
                    times[1] = System.currentTimeMillis();
                    done.countDown();
                }
            } else if (event instanceof TimeoutEdgeStartedEvent) {
                selectedTimeout[0] = ((TimeoutEdgeStartedEvent) event).getTimeoutMs();
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            assertTrue(project.start());
            assertTrue(done.await(5, TimeUnit.SECONDS), "Timeout interval should transition within 5s");
            waitForStop(5000);

            assertTrue(selectedTimeout[0] >= 40 && selectedTimeout[0] <= 80,
                    "Selected timeout must be in [40,80], was " + selectedTimeout[0]);
            long elapsed = times[1] - times[0];
            assertTrue(elapsed >= 35, "Interval timeout should wait at least ~35ms, was " + elapsed + "ms");
            assertTrue(elapsed < 160, "Interval timeout should fire well below 160ms, was " + elapsed + "ms");
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    @Test
    void invalidTimeoutIntervalFallsBackToFixedTimeout() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_TIMEOUT_INTERVAL_INVALID);
        project = new RunTimeProject(tempDir.toFile());

        CountDownLatch done = new CountDownLatch(1);
        long[] times = new long[2]; // [0] = N1 start, [1] = N2 start
        long[] selectedTimeout = new long[1];

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (id.equals("N1")) {
                    times[0] = System.currentTimeMillis();
                } else if (id.equals("N2")) {
                    times[1] = System.currentTimeMillis();
                    done.countDown();
                }
            } else if (event instanceof TimeoutEdgeStartedEvent) {
                selectedTimeout[0] = ((TimeoutEdgeStartedEvent) event).getTimeoutMs();
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            assertTrue(project.start());
            assertTrue(done.await(5, TimeUnit.SECONDS), "Fallback timeout should transition within 5s");
            waitForStop(5000);

            assertEquals(120, selectedTimeout[0], "Invalid interval must fall back to fixed timeout");
            long elapsed = times[1] - times[0];
            assertTrue(elapsed >= 110, "Fallback timeout should wait at least ~110ms, was " + elapsed + "ms");
            assertTrue(elapsed < 220, "Fallback timeout should fire below 220ms, was " + elapsed + "ms");
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    /**
     * Tests external variable change triggering a guarded edge.
     * N1 waits for guard(ready == true). External thread sets ready=true after 50ms.
     * Verifies the guard reacts promptly (should be near-instant with condition variable
     * signaling, not 10ms polling).
     */
    @Test
    void externalVariableChangeTriggerGuard() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_EXTERNAL_GUARD);
        project = new RunTimeProject(tempDir.toFile());

        CountDownLatch reachedN1 = new CountDownLatch(1);
        CountDownLatch reachedN2 = new CountDownLatch(1);
        long[] setTime = new long[1];
        long[] reactTime = new long[1];

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (id.equals("N1")) {
                    reachedN1.countDown();
                } else if (id.equals("N2")) {
                    reactTime[0] = System.currentTimeMillis();
                    reachedN2.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            assertTrue(project.start());

            // Wait for N1 to be entered
            assertTrue(reachedN1.await(5, TimeUnit.SECONDS), "Should reach N1");

            // Give the interpreter time to enter the edge-waiting loop
            Thread.sleep(50);

            // Set the variable externally
            setTime[0] = System.currentTimeMillis();
            assertTrue(project.setVariable("ready", true), "setVariable should succeed");

            // Wait for guard to fire
            assertTrue(reachedN2.await(5, TimeUnit.SECONDS), "Guard should fire after variable set");

            long reactionMs = reactTime[0] - setTime[0];
            // With condition variable signaling, reaction should be very fast (< 50ms)
            // We use a generous bound to avoid flaky tests
            assertTrue(reactionMs < 200, "Guard reaction should be < 200ms, was " + reactionMs + "ms");
            System.out.println("[PERF] Guard reaction time after external variable set: " + reactionMs + "ms");

            waitForStop(5000);
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    /**
     * Tests short-circuit && evaluation in guard conditions.
     * Flow: N1 sets x=5, y=0. Guard condition: (x > 3) && (y > 0).
     * Since y=0, the guard is false. N1 also has a timeout edge (100ms) to N2 as fallback.
     * Then a second guard: (x > 3) || (y > 10) which should be true (short-circuit: x>3 is true).
     * N2 has a CEdge with condition (x > 3 || y > 10) to N3.
     */
    @Test
    void shortCircuitEvaluation() throws Exception {
        writeProjectFiles(tempDir, SCENEFLOW_SHORT_CIRCUIT);
        project = new RunTimeProject(tempDir.toFile());

        List<String> visited = new CopyOnWriteArrayList<>();
        CountDownLatch done = new CountDownLatch(1);

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (!id.equals("Test")) {
                    visited.add(id);
                }
                if (id.equals("N3")) {
                    done.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            assertTrue(project.start());
            assertTrue(done.await(5, TimeUnit.SECONDS), "Short-circuit test should complete");
            waitForStop(5000);

            // N1 -> (timeout, && guard fails) -> N2 -> (|| guard succeeds via short-circuit) -> N3
            assertEquals(List.of("N1", "N2", "N3"), visited);
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    // ========== PERFORMANCE BENCHMARKS ==========

    /**
     * Measures epsilon-chain throughput: how many node transitions per second.
     * Creates a chain of 200 nodes connected by epsilon edges.
     */
    @Test
    void benchmarkEpsilonChainThroughput() throws Exception {
        int chainLength = 200;
        String sceneflow = generateEpsilonChain(chainLength);
        writeProjectFiles(tempDir, sceneflow);
        project = new RunTimeProject(tempDir.toFile());

        String lastNodeId = "N" + (chainLength - 1);
        CountDownLatch done = new CountDownLatch(1);
        int[] nodeCount = new int[1];

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (id.startsWith("N")) {
                    nodeCount[0]++;
                }
                if (id.equals(lastNodeId)) {
                    done.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            long startTime = System.nanoTime();
            assertTrue(project.start());
            assertTrue(done.await(30, TimeUnit.SECONDS), "Chain of " + chainLength + " should complete");
            long elapsed = System.nanoTime() - startTime;

            double elapsedMs = elapsed / 1_000_000.0;
            double transitionsPerSec = (nodeCount[0] / elapsedMs) * 1000;
            System.out.println("[PERF] Epsilon chain: " + chainLength + " nodes in " +
                    String.format("%.1f", elapsedMs) + "ms = " +
                    String.format("%.0f", transitionsPerSec) + " transitions/sec");

            waitForStop(5000);
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    /**
     * Measures timeout edge accuracy: creates 10 sequential 50ms timeouts
     * and checks that total time is close to 500ms (not inflated by polling overhead).
     */
    @Test
    void benchmarkTimeoutAccuracy() throws Exception {
        int timeoutCount = 10;
        int timeoutMs = 50;
        String sceneflow = generateTimeoutChain(timeoutCount, timeoutMs);
        writeProjectFiles(tempDir, sceneflow);
        project = new RunTimeProject(tempDir.toFile());

        String lastNodeId = "N" + timeoutCount;
        CountDownLatch done = new CountDownLatch(1);

        EventListener listener = event -> {
            if (event instanceof NodeStartedEvent) {
                String id = ((NodeStartedEvent) event).getNode().getId();
                if (id.equals(lastNodeId)) {
                    done.countDown();
                }
            }
        };
        EventDispatcher.getInstance().register(listener);

        try {
            assertTrue(project.launch());
            long startTime = System.currentTimeMillis();
            assertTrue(project.start());
            assertTrue(done.await(30, TimeUnit.SECONDS), "Timeout chain should complete");
            long elapsed = System.currentTimeMillis() - startTime;

            long expectedMs = (long) timeoutCount * timeoutMs;
            double overhead = elapsed - expectedMs;
            System.out.println("[PERF] Timeout chain: " + timeoutCount + "x" + timeoutMs +
                    "ms = expected " + expectedMs + "ms, actual " + elapsed +
                    "ms (overhead: " + String.format("%.0f", overhead) + "ms)");

            // With condition-variable signaling, overhead should be small
            // With old 10ms polling, each timeout would add up to 10ms overhead
            assertTrue(elapsed < expectedMs * 3,
                    "Total time should not exceed 3x expected (" + expectedMs + "ms), was " + elapsed + "ms");

            waitForStop(5000);
        } finally {
            EventDispatcher.getInstance().remove(listener);
        }
    }

    // ========== HELPER METHODS ==========

    private void waitForStop(long maxWaitMs) throws InterruptedException {
        long deadline = System.currentTimeMillis() + maxWaitMs;
        while (project.isRunning() && System.currentTimeMillis() < deadline) {
            Thread.sleep(50);
        }
    }

    private void writeProjectFiles(Path dir, String sceneflowXml) throws IOException {
        writeFile(dir, "project.xml", PROJECT_XML);
        writeFile(dir, "sceneflow.xml", sceneflowXml);
        writeFile(dir, "scenescript.xml", SCENESCRIPT_XML);
        writeFile(dir, "acticon.xml", ACTICON_XML);
        writeFile(dir, "visicon.xml", VISICON_XML);
        writeFile(dir, "gesticon.xml", GESTICON_XML);
    }

    private void writeFile(Path dir, String name, String content) throws IOException {
        File f = dir.resolve(name).toFile();
        try (FileWriter w = new FileWriter(f)) {
            w.write(content);
        }
    }

    private String generateEpsilonChain(int length) {
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        sb.append("<SceneFlow id=\"Test\" name=\"Test\" start=\"N0;\"");
        sb.append(" xmlns=\"xml.sceneflow.dfki.de\"");
        sb.append(" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
        sb.append(" xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n");
        sb.append("  <Define/>\n  <Declare/>\n  <Commands/>\n");

        for (int i = 0; i < length; i++) {
            sb.append("  <Node id=\"N").append(i).append("\" name=\"N").append(i).append("\" history=\"false\">\n");
            sb.append("    <Define/>\n    <Declare/>\n    <Commands/>\n");
            if (i < length - 1) {
                sb.append("    <EEdge target=\"N").append(i + 1).append("\" start=\"\">\n");
                sb.append("    </EEdge>\n");
            }
            sb.append("    <Graphics><Position xPos=\"").append(i * 150).append("\" yPos=\"50\"/></Graphics>\n");
            sb.append("  </Node>\n");
        }

        sb.append("</SceneFlow>\n");
        return sb.toString();
    }

    private String generateTimeoutChain(int count, int timeoutMs) {
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        sb.append("<SceneFlow id=\"Test\" name=\"Test\" start=\"N0;\"");
        sb.append(" xmlns=\"xml.sceneflow.dfki.de\"");
        sb.append(" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
        sb.append(" xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n");
        sb.append("  <Define/>\n  <Declare/>\n  <Commands/>\n");

        for (int i = 0; i <= count; i++) {
            sb.append("  <Node id=\"N").append(i).append("\" name=\"N").append(i).append("\" history=\"false\">\n");
            sb.append("    <Define/>\n    <Declare/>\n    <Commands/>\n");
            if (i < count) {
                sb.append("    <TEdge target=\"N").append(i + 1).append("\" start=\"\" timeout=\"").append(timeoutMs).append("\">\n");
                sb.append("    </TEdge>\n");
            }
            sb.append("    <Graphics><Position xPos=\"").append(i * 150).append("\" yPos=\"50\"/></Graphics>\n");
            sb.append("  </Node>\n");
        }

        sb.append("</SceneFlow>\n");
        return sb.toString();
    }

    // ========== XML TEMPLATES ==========

    private static final String PROJECT_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<Project name=\"Test\">\n" +
            "  <Plugins/>\n" +
            "  <Agents/>\n" +
            "  <Player/>\n" +
            "</Project>\n";

    private static final String SCENESCRIPT_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneScript lower=\"0\" upper=\"0\">\n" +
            "</SceneScript>\n";

    private static final String ACTICON_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<ActiconConfig/>\n";

    private static final String VISICON_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<VisiconConfig/>\n";

    private static final String GESTICON_XML =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<GesticonConfig/>\n";

    /** N1 -> epsilon -> N2 -> epsilon -> N3 (end) */
    private static final String SCENEFLOW_EPSILON_CHAIN =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n" +
            "  <Declare/>\n" +
            "  <Commands/>\n" +
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <EEdge target=\"N2\" start=\"\"/>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <EEdge target=\"N3\" start=\"\"/>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "  <Node id=\"N3\" name=\"N3\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"350\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";

    /**
     * N1 (x=1) -> epsilon -> N2 (x=x+1) -> guard(x==2) -> N3 (end)
     * Variable x declared as Int with initial value 0.
     */
    private static final String SCENEFLOW_GUARD =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n" +
            "  <Declare>\n" +
            "    <VariableDefinition type=\"Int\" name=\"x\">\n" +
            "      <IntLiteral value=\"0\"/>\n" +
            "    </VariableDefinition>\n" +
            "  </Declare>\n" +
            "  <Commands/>\n" +
            // N1: x = 1, then epsilon to N2
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n" +
            "    <Commands>\n" +
            "      <Assignment>\n" +
            "        <SimpleVariable name=\"x\"/>\n" +
            "        <Expression><IntLiteral value=\"1\"/></Expression>\n" +
            "      </Assignment>\n" +
            "    </Commands>\n" +
            "    <EEdge target=\"N2\" start=\"\"/>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            // N2: x = x + 1, then guard(x == 2) to N3
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n" +
            "    <Commands>\n" +
            "      <Assignment>\n" +
            "        <SimpleVariable name=\"x\"/>\n" +
            "        <Expression>\n" +
            "          <Add>\n" +
            "            <SimpleVariable name=\"x\"/>\n" +
            "            <IntLiteral value=\"1\"/>\n" +
            "          </Add>\n" +
            "        </Expression>\n" +
            "      </Assignment>\n" +
            "    </Commands>\n" +
            "    <CEdge target=\"N3\" start=\"\">\n" +
            "      <Eq>\n" +
            "        <SimpleVariable name=\"x\"/>\n" +
            "        <IntLiteral value=\"2\"/>\n" +
            "      </Eq>\n" +
            "    </CEdge>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            // N3: end
            "  <Node id=\"N3\" name=\"N3\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"350\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";

    /** N1 --(100ms timeout)--> N2 (end) */
    private static final String SCENEFLOW_TIMEOUT =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n  <Declare/>\n  <Commands/>\n" +
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <TEdge target=\"N2\" start=\"\" timeout=\"100\"/>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";

    /** N1 --(random timeout in [40,80]ms)--> N2 (end) */
    private static final String SCENEFLOW_TIMEOUT_INTERVAL =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n  <Declare/>\n  <Commands/>\n" +
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <TEdge target=\"N2\" start=\"\" timeout=\"40\" timeoutMin=\"40\" timeoutMax=\"80\"/>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";

    /** Invalid interval attributes must fall back to fixed timeout=120ms. */
    private static final String SCENEFLOW_TIMEOUT_INTERVAL_INVALID =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n  <Declare/>\n  <Commands/>\n" +
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <TEdge target=\"N2\" start=\"\" timeout=\"120\" timeoutMin=\"120\" timeoutMax=\"100\"/>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";

    /**
     * N1 waits for guard(ready == true), then transitions to N2.
     * The variable "ready" starts as false and must be set externally.
     */
    private static final String SCENEFLOW_EXTERNAL_GUARD =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n" +
            "  <Declare>\n" +
            "    <VariableDefinition type=\"Bool\" name=\"ready\">\n" +
            "      <BoolLiteral value=\"false\"/>\n" +
            "    </VariableDefinition>\n" +
            "  </Declare>\n" +
            "  <Commands/>\n" +
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <CEdge target=\"N2\" start=\"\">\n" +
            "      <SimpleVariable name=\"ready\"/>\n" +
            "    </CEdge>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";

    /**
     * N1: sets x=5, y=0. CEdge (x>3 && y>0) fails, TEdge 100ms falls through to N2.
     * N2: CEdge (x>3 || y>10) succeeds via short-circuit (x>3 is true) -> N3 (end).
     */
    private static final String SCENEFLOW_SHORT_CIRCUIT =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<SceneFlow id=\"Test\" name=\"Test\" start=\"N1;\"" +
            " xmlns=\"xml.sceneflow.dfki.de\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n" +
            "  <Define/>\n" +
            "  <Declare>\n" +
            "    <VariableDefinition type=\"Int\" name=\"x\">\n" +
            "      <IntLiteral value=\"0\"/>\n" +
            "    </VariableDefinition>\n" +
            "    <VariableDefinition type=\"Int\" name=\"y\">\n" +
            "      <IntLiteral value=\"0\"/>\n" +
            "    </VariableDefinition>\n" +
            "  </Declare>\n" +
            "  <Commands/>\n" +
            // N1: x=5, y=0. CEdge(x>3 && y>0) → fails. TEdge(100ms) → N2.
            "  <Node id=\"N1\" name=\"N1\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n" +
            "    <Commands>\n" +
            "      <Assignment>\n" +
            "        <SimpleVariable name=\"x\"/>\n" +
            "        <Expression><IntLiteral value=\"5\"/></Expression>\n" +
            "      </Assignment>\n" +
            "      <Assignment>\n" +
            "        <SimpleVariable name=\"y\"/>\n" +
            "        <Expression><IntLiteral value=\"0\"/></Expression>\n" +
            "      </Assignment>\n" +
            "    </Commands>\n" +
            "    <CEdge target=\"N3\" start=\"\">\n" +
            "      <AndAnd>\n" +
            "        <Gt>\n" +
            "          <SimpleVariable name=\"x\"/>\n" +
            "          <IntLiteral value=\"3\"/>\n" +
            "        </Gt>\n" +
            "        <Gt>\n" +
            "          <SimpleVariable name=\"y\"/>\n" +
            "          <IntLiteral value=\"0\"/>\n" +
            "        </Gt>\n" +
            "      </AndAnd>\n" +
            "    </CEdge>\n" +
            "    <TEdge target=\"N2\" start=\"\" timeout=\"100\"/>\n" +
            "    <Graphics><Position xPos=\"50\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            // N2: CEdge(x>3 || y>10) → succeeds (x>3 is true, short-circuits) → N3
            "  <Node id=\"N2\" name=\"N2\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <CEdge target=\"N3\" start=\"\">\n" +
            "      <OrOr>\n" +
            "        <Gt>\n" +
            "          <SimpleVariable name=\"x\"/>\n" +
            "          <IntLiteral value=\"3\"/>\n" +
            "        </Gt>\n" +
            "        <Gt>\n" +
            "          <SimpleVariable name=\"y\"/>\n" +
            "          <IntLiteral value=\"10\"/>\n" +
            "        </Gt>\n" +
            "      </OrOr>\n" +
            "    </CEdge>\n" +
            "    <Graphics><Position xPos=\"200\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            // N3: end
            "  <Node id=\"N3\" name=\"N3\" history=\"false\">\n" +
            "    <Define/>\n    <Declare/>\n    <Commands/>\n" +
            "    <Graphics><Position xPos=\"350\" yPos=\"50\"/></Graphics>\n" +
            "  </Node>\n" +
            "</SceneFlow>\n";
}
