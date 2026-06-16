package de.dfki.vsm.benchmark;

import de.dfki.vsm.runtime.CoreRuntime;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

class LatencyBenchmark {

    private static final String LOOP_NODE_ID = "N2";
    private static final int WARMUP_ITERATIONS = 10;

    static void run(File projectDir, int[] counts, int iterations, int timeoutMs) {
        System.out.println("Running latency benchmark (" + iterations + " samples per project, "
                + timeoutMs + " ms timeout)...");

        long[][] deviationsPerCount = new long[counts.length][];

        for (int i = 0; i < counts.length; i++) {
            int n = counts[i];
            System.out.print("  " + n + " concurrent project(s)... ");

            List<CoreRuntime> projects = new ArrayList<>(n);
            List<LatencyProbe> probes = new ArrayList<>(n);

            try {
                for (int j = 0; j < n; j++) {
                    CoreRuntime rt = new CoreRuntime(projectDir);
                    LatencyProbe probe = new LatencyProbe(LOOP_NODE_ID, timeoutMs, WARMUP_ITERATIONS);
                    rt.getRunTimeProject().getEventDispatcher().register(probe);
                    if (rt.launch()) rt.start();
                    projects.add(rt);
                    probes.add(probe);
                }

                // Wait for (warmup + measurement) iterations to complete
                long waitMs = (long) (WARMUP_ITERATIONS + iterations) * timeoutMs + 2_000;
                Thread.sleep(waitMs);

                // Aggregate deviations from all probes
                List<Long> allDeviations = new ArrayList<>();
                for (LatencyProbe probe : probes) {
                    allDeviations.addAll(probe.getDeviations());
                }
                deviationsPerCount[i] = BenchmarkReport.toLongArray(allDeviations);
                System.out.println(deviationsPerCount[i].length + " samples");

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                deviationsPerCount[i] = new long[0];
            } finally {
                for (CoreRuntime rt : projects) rt.shutdown();
                MemoryBenchmark.forceGC();
            }
        }

        System.out.println();
        BenchmarkReport.printLatencyTable(counts, deviationsPerCount, timeoutMs);
    }
}
