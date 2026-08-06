package de.dfki.vsm.web;

import de.dfki.vsm.model.behavior.placement.PlacementContext;
import de.dfki.vsm.model.behavior.placement.PlacementModel;
import de.dfki.vsm.model.behavior.placement.PlacementSuggestion;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Leave-one-out evaluation of the placement model over a corpus JSONL (plan 3.5).
 *
 * <pre>
 *   ./gradlew placementEval -Pcorpus=/path/to/corpus.jsonl
 * </pre>
 *
 * <p>For each co-speech placement, train on every other placement and predict this one. Reported
 * against three baselines, because a top-1 number alone says nothing: a model that always answers
 * "utterance-initial" scores surprisingly well on short utterances, and beating uniform-random is
 * not an achievement.
 *
 * <p>The honest caveat is printed with the results and belongs in any write-up: at a few dozen
 * placements this is a smoke test for wiring and gross errors, not a measurement of accuracy.
 */
public final class PlacementEvalCli {

    /** One test item: the context to predict for, the slots available, and what the author chose. */
    private static final class Item {
        private PlacementContext context;
        private List<String> offered;
        private String gold;
        private String function;
    }

    private PlacementEvalCli() {
    }

    public static void main(final String[] args) throws Exception {
        String corpusPath = null;
        for (final String arg : args) {
            if (arg.startsWith("--corpus=")) {
                corpusPath = arg.substring("--corpus=".length());
            }
        }
        if (corpusPath == null || corpusPath.isBlank()) {
            System.err.println("usage: PlacementEvalCli --corpus=<corpus.jsonl>");
            System.exit(2);
            return;
        }

        final List<Item> items = load(Path.of(corpusPath));
        if (items.isEmpty()) {
            System.err.println("no usable placements in " + corpusPath
                    + " — need co-speech placements that sit on an anchor slot");
            System.exit(1);
            return;
        }

        System.out.println("Placement model — leave-one-out evaluation");
        System.out.println("========================================================================");
        System.out.println("corpus            " + corpusPath);
        System.out.println("evaluable items   " + items.size()
                + "   <- co-speech placements that landed on a slot");

        int top1 = 0;
        int top3 = 0;
        int priorOnly = 0;
        int baselineInitial = 0;
        int baselinePrior = 0;
        double baselineRandom = 0.0;
        final Map<String, int[]> perFunction = new LinkedHashMap<>();

        for (int held = 0; held < items.size(); held += 1) {
            final Item test = items.get(held);
            final PlacementModel model = PlacementModel.empty();
            for (int i = 0; i < items.size(); i += 1) {
                if (i != held) {
                    model.observe(items.get(i).context, items.get(i).gold);
                }
            }
            final List<PlacementSuggestion> ranked = model.suggest(test.context, test.offered, 3);
            final boolean hit1 = !ranked.isEmpty() && test.gold.equals(ranked.get(0).getSlot());
            boolean hit3 = false;
            for (final PlacementSuggestion suggestion : ranked) {
                if (test.gold.equals(suggestion.getSlot())) {
                    hit3 = true;
                    break;
                }
            }
            if (hit1) {
                top1 += 1;
            }
            if (hit3) {
                top3 += 1;
            }
            if (!ranked.isEmpty() && ranked.get(0).isPriorOnly()) {
                priorOnly += 1;
            }

            // Baselines.
            if ("utterance-initial".equals(test.gold) && test.offered.contains("utterance-initial")) {
                baselineInitial += 1;
            }
            baselineRandom += 1.0 / test.offered.size();
            final List<PlacementSuggestion> priorRanked =
                    PlacementModel.empty().suggest(test.context, test.offered, 1);
            if (!priorRanked.isEmpty() && test.gold.equals(priorRanked.get(0).getSlot())) {
                baselinePrior += 1;
            }

            final String key = test.function == null ? "(none)" : test.function;
            final int[] tally = perFunction.computeIfAbsent(key, k -> new int[2]);
            tally[0] += hit1 ? 1 : 0;
            tally[1] += 1;
        }

        final int n = items.size();
        System.out.println();
        System.out.printf(Locale.ROOT, "top-1 exact slot   %d/%d  %.1f%%%n", top1, n, pct(top1, n));
        System.out.printf(Locale.ROOT, "top-3 contains     %d/%d  %.1f%%%n", top3, n, pct(top3, n));
        System.out.printf(Locale.ROOT, "decided by prior   %d/%d  %.1f%%   "
                + "<- contexts the corpus has never shown%n", priorOnly, n, pct(priorOnly, n));
        System.out.println();
        System.out.println("baselines");
        System.out.printf(Locale.ROOT, "  always utterance-initial  %d/%d  %.1f%%%n",
                baselineInitial, n, pct(baselineInitial, n));
        System.out.printf(Locale.ROOT, "  prior only (no corpus)    %d/%d  %.1f%%%n",
                baselinePrior, n, pct(baselinePrior, n));
        System.out.printf(Locale.ROOT, "  uniform random            %.1f/%d  %.1f%%%n",
                baselineRandom, n, 100.0 * baselineRandom / n);

        System.out.println();
        System.out.println("top-1 by NEUROGES Function");
        for (final Map.Entry<String, int[]> entry : perFunction.entrySet()) {
            final int[] tally = entry.getValue();
            System.out.printf(Locale.ROOT, "  %-32s %d/%d  %.0f%%%n",
                    entry.getKey(), tally[0], tally[1], pct(tally[0], tally[1]));
        }

        System.out.println();
        if (n < 50) {
            System.out.println("VERDICT: " + n + " items is a smoke test, not a measurement. It shows the");
            System.out.println("wiring works and catches gross errors. Do not quote these percentages as");
            System.out.println("accuracy — leave-one-out on a corpus this small has a variance far wider");
            System.out.println("than the gaps between the numbers above.");
        } else {
            System.out.println("VERDICT: " + n + " items — usable as a trend, still small. Report n with any figure.");
        }
    }

    private static double pct(final int hit, final int total) {
        return total == 0 ? 0.0 : 100.0 * hit / total;
    }

    private static List<Item> load(final Path path) throws Exception {
        final List<Item> out = new ArrayList<>();
        // Sentences per turn, so a sentence's position in its turn can be derived. The corpus records
        // sentences, not turns, so they are regrouped by (project, line) — one script line is one turn.
        final Map<String, Integer> turnSizes = new LinkedHashMap<>();
        final List<JSONObject> records = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                final String trimmed = line.trim();
                if (trimmed.isEmpty()) {
                    continue;
                }
                final JSONObject record = new JSONObject(trimmed);
                records.add(record);
                turnSizes.merge(turnKey(record), 1, Integer::sum);
            }
        }

        final Map<String, Integer> seenInTurn = new LinkedHashMap<>();
        for (final JSONObject record : records) {
            final String turnKey = turnKey(record);
            final int indexInTurn = seenInTurn.merge(turnKey, 1, Integer::sum) - 1;
            final PlacementContext.TurnPosition position =
                    PlacementContext.TurnPosition.of(indexInTurn, turnSizes.getOrDefault(turnKey, 1));

            final List<String> offered = new ArrayList<>();
            final JSONArray anchors = record.optJSONArray("anchors");
            if (anchors != null) {
                for (int i = 0; i < anchors.length(); i += 1) {
                    final JSONObject anchor = anchors.optJSONObject(i);
                    if (anchor == null) {
                        continue;
                    }
                    final String slot = anchor.optString("slot", "");
                    if (!slot.isEmpty() && !offered.contains(slot)) {
                        offered.add(slot);
                    }
                }
            }
            if (offered.isEmpty()) {
                continue;
            }

            final JSONArray placements = record.optJSONArray("placements");
            if (placements == null) {
                continue;
            }
            for (int i = 0; i < placements.length(); i += 1) {
                final JSONObject placement = placements.optJSONObject(i);
                if (placement == null || !placement.optBoolean("cospeech", false)) {
                    continue;
                }
                final Object anchor = placement.opt("anchor");
                if (!(anchor instanceof String) || ((String) anchor).isEmpty()) {
                    // Mid-phrase placements are excluded rather than snapped. Snapping here would
                    // manufacture agreement between the model and a gold label that the corpus does
                    // not actually contain, which is the one thing an evaluation must not do.
                    continue;
                }
                final Item item = new Item();
                item.gold = (String) anchor;
                item.offered = offered;
                item.function = placement.optString("function", null);
                item.context = new PlacementContext(
                        item.function,
                        placement.optString("affiliate", null),
                        clauseTypeOf(record, placement.optString("clauseId", "")),
                        position,
                        dialogueActOf(record));
                out.add(item);
            }
        }
        return out;
    }

    private static String turnKey(final JSONObject record) {
        return record.optString("project", "") + "#" + record.optString("scenario", "")
                + "#" + record.optInt("line", 0);
    }

    private static String clauseTypeOf(final JSONObject record, final String clauseId) {
        final JSONArray clauses = record.optJSONArray("clauses");
        if (clauses == null || clauseId == null || clauseId.isEmpty()) {
            return null;
        }
        for (int i = 0; i < clauses.length(); i += 1) {
            final JSONObject clause = clauses.optJSONObject(i);
            if (clause != null && clauseId.equals(clause.optString("id", ""))) {
                final String type = clause.optString("type", "");
                return type.isEmpty() ? null : type;
            }
        }
        return null;
    }

    private static String dialogueActOf(final JSONObject record) {
        final JSONObject analysis = record.optJSONObject("analysis");
        if (analysis == null) {
            return null;
        }
        final JSONObject act = analysis.optJSONObject("dialogueAct");
        if (act == null) {
            return null;
        }
        final String label = act.optString("label", "");
        return label.isEmpty() ? null : label;
    }
}
