package de.dfki.vsm.benchmark;

import de.dfki.vsm.runtime.CoreRuntime;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

class MemoryBenchmark {

    static void run(File projectDir, int[] counts) {
        System.out.println("Running memory benchmark...");

        long baselineMB = measureHeapMB();
        long[] heapMB = new long[counts.length];

        for (int i = 0; i < counts.length; i++) {
            int n = counts[i];
            System.out.print("  Starting " + n + " project(s)... ");
            List<CoreRuntime> projects = new ArrayList<>(n);
            try {
                for (int j = 0; j < n; j++) {
                    CoreRuntime rt = new CoreRuntime(projectDir);
                    if (rt.launch()) rt.start();
                    projects.add(rt);
                }
                // Allow initialization to settle
                Thread.sleep(2_000);
                heapMB[i] = measureHeapMB();
                System.out.println(heapMB[i] + " MB heap");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                for (CoreRuntime rt : projects) rt.shutdown();
                // Recover heap before next tier
                forceGC();
            }
        }

        System.out.println();
        BenchmarkReport.printMemoryTable(counts, heapMB, baselineMB);
    }

    private static long measureHeapMB() {
        forceGC();
        Runtime rt = Runtime.getRuntime();
        return (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024);
    }

    static void forceGC() {
        for (int i = 0; i < 3; i++) {
            System.gc();
            try { Thread.sleep(100); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }
        }
    }
}
