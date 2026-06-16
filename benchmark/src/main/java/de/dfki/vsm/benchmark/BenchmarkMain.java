package de.dfki.vsm.benchmark;

import de.dfki.vsm.runtime.bootstrap.PlatformBootstrap;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * VSM Runtime Benchmark
 *
 * Measures two things:
 *   1. Memory — heap consumed per concurrent RunTimeProject (no plugins, pure interpreter)
 *   2. Latency — timing deviation of the interpreter loop relative to its scheduled timeout
 *
 * Usage (via Gradle):
 *   ./gradlew :benchmark:runBenchmark [-PprojectCounts=1,10,50,100] [-Piterations=100] [-Ptimeout=200] [-Pmode=all|memory|latency]
 *
 * Usage (direct jar):
 *   java -jar benchmark.jar [--projects=1,10,50,100] [--iterations=100] [--timeout=200] [--mode=all|memory|latency]
 *
 * Recommended JVM flags for multi-tier runs:
 *   -Xmx4g -Xlog:gc:file=benchmark/gc.log
 *
 * For production-scale testing (> 50 projects):
 *   -Xmx8g -XX:+UseZGC
 */
public class BenchmarkMain {

    private static final int DEFAULT_TIMEOUT_MS = 200;

    public static void main(String[] args) {
        // Suppress VSM runtime logging — it saturates the console and skews timing
        Logger.getLogger("").setLevel(Level.OFF);
        PlatformBootstrap.configureForCurrentVm();

        // Defaults
        int[] projectCounts = {1, 10, 50, 100};
        int iterations = 100;
        int timeoutMs = DEFAULT_TIMEOUT_MS;
        String mode = "all";

        for (String arg : args) {
            if (arg.startsWith("--projects=")) {
                projectCounts = parseCsvInts(arg.substring("--projects=".length()));
            } else if (arg.startsWith("--iterations=")) {
                iterations = Integer.parseInt(arg.substring("--iterations=".length()));
            } else if (arg.startsWith("--timeout=")) {
                timeoutMs = Integer.parseInt(arg.substring("--timeout=".length()));
            } else if (arg.startsWith("--mode=")) {
                mode = arg.substring("--mode=".length());
            }
        }

        File projectDir = resolveProjectDir();

        System.out.println("=".repeat(72));
        System.out.println("  VSM Runtime Benchmark");
        System.out.println("  Project dir : " + projectDir.getAbsolutePath());
        System.out.println("  Mode        : " + mode);
        System.out.println("  Iterations  : " + iterations);
        System.out.println("  Timeout     : " + timeoutMs + " ms");
        System.out.println("  JVM heap    : " + Runtime.getRuntime().maxMemory() / (1024 * 1024) + " MB max");
        System.out.println("=".repeat(72));
        System.out.println();

        if (!projectDir.isDirectory()) {
            System.err.println("ERROR: benchmark project directory not found: " + projectDir.getAbsolutePath());
            System.err.println("Run from the VisualSceneMaker root, or pass the project dir as --projectDir=<path>.");
            System.exit(1);
        }

        if (mode.equals("memory") || mode.equals("all")) {
            MemoryBenchmark.run(projectDir, projectCounts);
        }
        if (mode.equals("latency") || mode.equals("all")) {
            LatencyBenchmark.run(projectDir, projectCounts, iterations, timeoutMs);
        }

        System.out.println("Benchmark complete.");
    }

    private static File resolveProjectDir() {
        // When run via Gradle from the project root, "benchmark/minimal" is accessible directly.
        // When the jar is executed from elsewhere, fall back to the directory next to the jar.
        File f = new File("benchmark/minimal");
        if (f.isDirectory()) return f;
        // Try one level up (in case cwd is benchmark/)
        f = new File("minimal");
        if (f.isDirectory()) return f;
        return new File("benchmark/minimal");
    }

    private static int[] parseCsvInts(String csv) {
        String[] parts = csv.split(",");
        int[] result = new int[parts.length];
        for (int i = 0; i < parts.length; i++) result[i] = Integer.parseInt(parts[i].trim());
        return result;
    }
}
