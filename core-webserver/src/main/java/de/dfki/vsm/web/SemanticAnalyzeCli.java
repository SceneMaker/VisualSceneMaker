package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Headless semantic analysis of one or more projects — no browser, no editor.
 *
 * <p>This is the corpus tool's entry point: it boots {@link WebUiServer} in-process purely to reuse
 * the project loader and the analysis pipeline, analyses each project's script, optionally writes
 * {@code semantic-annotations.json} next to the project, prints a summary, and exits. Nothing is
 * served — the server is never started, so no port is bound and no browser is opened.</p>
 *
 * <pre>
 * ./gradlew analyzeSemantics -PvsmProject=/path/to/ProjectDir
 * ./gradlew analyzeSemantics -PvsmProject=/a,/b,/c -Ppersist=true -Planguage=de
 * </pre>
 *
 * <p>({@code -PvsmProject}, not {@code -Pproject}: the latter is Gradle's own Project object.)</p>
 *
 * <p>Requires the UD service to be reachable (default {@code http://127.0.0.1:4061}); start it with
 * {@code ./gradlew :services:semantic-ud:startService}. Without it the basic layer is simply absent
 * and each sentence is reported as a warning, rather than the run failing.</p>
 *
 * <p>Exit codes: 0 all projects analysed, 1 usage error, 2 at least one project failed.</p>
 *
 * @author Patrick Gebhard
 */
public final class SemanticAnalyzeCli {

    private SemanticAnalyzeCli() {
    }

    public static void main(String[] args) {
        List<String> projects = new ArrayList<>();
        boolean persist = false;
        boolean debug = false;
        String language = "de";
        boolean wantDaTr = false;

        for (String arg : args) {
            if (arg.startsWith("--project=")) {
                for (String part : arg.substring("--project=".length()).split(",")) {
                    if (!part.isBlank()) {
                        projects.add(part.trim());
                    }
                }
            } else if (arg.equals("--persist")) {
                persist = true;
            } else if (arg.equals("--debug")) {
                debug = true;
            } else if (arg.equals("--da-tr")) {
                wantDaTr = true;
            } else if (arg.startsWith("--language=")) {
                language = arg.substring("--language=".length()).trim();
            } else {
                System.err.println("Unknown argument: " + arg);
                usage();
                System.exit(1);
            }
        }
        if (projects.isEmpty()) {
            usage();
            System.exit(1);
        }

        JSONObject options = new JSONObject()
                .put("layers", new JSONObject()
                        .put("basic", true)
                        .put("dialogueAct", wantDaTr)
                        .put("themeRheme", wantDaTr))
                .put("useLlm", wantDaTr)
                .put("language", language)
                .put("persist", persist)
                .put("debug", debug);

        // Singleton, but never started: we only want its project loader and analysis pipeline, so no
        // port is bound and no browser is opened.
        WebUiServer server = WebUiServer.getInstance();
        int failed = 0;

        for (String projectPath : projects) {
            System.out.println("== " + projectPath);
            if (!Files.isDirectory(Path.of(projectPath))) {
                System.err.println("   not a directory");
                failed += 1;
                continue;
            }
            try {
                // Parse-only: never launches the project's plugins, so no ports are bound and a
                // running editor is undisturbed.
                String pid = server.loadProjectForAnalysis(projectPath);
                if (pid == null) {
                    System.err.println("   could not parse project");
                    failed += 1;
                    continue;
                }
                long started = System.currentTimeMillis();
                JSONObject doc = server.analyzeScriptSemantics(pid, options);
                long elapsed = System.currentTimeMillis() - started;

                JSONObject stats = doc.optJSONObject("stats");
                System.out.printf("   sentences=%d annotations=%d commands=%d  (%.1fs)%n",
                        stats == null ? 0 : stats.optInt("sentences"),
                        stats == null ? 0 : stats.optInt("annotations"),
                        stats == null ? 0 : stats.optInt("commands"),
                        elapsed / 1000.0);

                JSONArray warnings = doc.optJSONArray("warnings");
                if (warnings != null) {
                    for (int i = 0; i < Math.min(warnings.length(), 5); i++) {
                        System.out.println("   warning: " + warnings.optString(i));
                    }
                    if (warnings.length() > 5) {
                        System.out.println("   … " + (warnings.length() - 5) + " more warnings");
                    }
                }
                if (persist) {
                    System.out.println("   wrote semantic-annotations.json");
                }
            } catch (WebUiServer.SemanticAnalysisException exc) {
                System.err.println("   " + exc.code + ": " + exc.getMessage());
                failed += 1;
            } catch (Exception exc) {
                System.err.println("   failed: " + exc);
                failed += 1;
            }
        }

        System.out.println();
        System.out.printf("%d of %d project(s) analysed%n", projects.size() - failed, projects.size());
        System.exit(failed == 0 ? 0 : 2);
    }

    private static void usage() {
        System.err.println("Usage: SemanticAnalyzeCli --project=<dir>[,<dir>…] "
                + "[--persist] [--da-tr] [--language=de] [--debug]");
        System.err.println();
        System.err.println("  --persist     write semantic-annotations.json into each project");
        System.err.println("  --da-tr       also run the LLM dialogue-act / theme-rheme layers");
        System.err.println("  --language    fallback language for scenes that declare none");
        System.err.println("  --debug       include UD parse traces in the document");
    }
}
