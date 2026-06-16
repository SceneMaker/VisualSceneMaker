package de.dfki.vsm.benchmark;

import java.util.Arrays;
import java.util.List;

class BenchmarkReport {

    static void printMemoryTable(int[] counts, long[] heapMB, long baselineMB) {
        System.out.println("=== Memory Benchmark ===");
        System.out.printf("%-20s  %-12s  %-15s  %-15s%n",
                "Concurrent projects", "Heap (MB)", "Overhead (MB)", "Per-project (MB)");
        System.out.println("-".repeat(68));
        for (int i = 0; i < counts.length; i++) {
            long overhead = heapMB[i] - baselineMB;
            long perProject = counts[i] > 0 ? overhead / counts[i] : 0;
            System.out.printf("%-20d  %-12d  %-15d  %-15d%n",
                    counts[i], heapMB[i], overhead, perProject);
        }
        System.out.printf("  (JVM baseline: %d MB)%n", baselineMB);
        System.out.println();
    }

    static void printLatencyTable(int[] counts, long[][] deviationsPerCount, int timeoutMs) {
        System.out.println("=== Latency Benchmark (timeout=" + timeoutMs + " ms) ===");
        System.out.printf("%-20s  %-8s  %-8s  %-8s  %-8s  %-8s  %-8s%n",
                "Concurrent projects", "samples", "p50", "p90", "p95", "p99", "max");
        System.out.println("  (all deviation values in ms; positive = fired late)");
        System.out.println("-".repeat(76));
        for (int i = 0; i < counts.length; i++) {
            long[] devs = deviationsPerCount[i];
            if (devs.length == 0) {
                System.out.printf("%-20d  (no samples collected)%n", counts[i]);
                continue;
            }
            Arrays.sort(devs);
            System.out.printf("%-20d  %-8d  %-8d  %-8d  %-8d  %-8d  %-8d%n",
                    counts[i],
                    devs.length,
                    percentile(devs, 50),
                    percentile(devs, 90),
                    percentile(devs, 95),
                    percentile(devs, 99),
                    devs[devs.length - 1]);
        }
        System.out.println();
    }

    private static long percentile(long[] sorted, int pct) {
        int idx = (int) Math.ceil(pct / 100.0 * sorted.length) - 1;
        return sorted[Math.max(0, Math.min(idx, sorted.length - 1))];
    }

    static long[] toLongArray(List<Long> list) {
        long[] arr = new long[list.size()];
        for (int i = 0; i < list.size(); i++) arr[i] = list.get(i);
        return arr;
    }
}
