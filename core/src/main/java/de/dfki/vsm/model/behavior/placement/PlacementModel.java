package de.dfki.vsm.model.behavior.placement;

import de.dfki.vsm.model.behavior.BehaviorTaxonomy;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * Per-project placement model: where this project's author puts each kind of behavior command.
 *
 * <p>A count model with hierarchical back-off, chosen over anything trained because the corpus is
 * tiny — 38 co-speech placements across four projects at the time of writing — and because an author
 * must be able to see why a suggestion was made. Every number in {@code behavior-placement.json} is
 * a tally of things that author actually did.
 *
 * <p>The back-off chain runs from the most specific context to a hand-written prior, so the model is
 * useful at n=0 and sharpens as the project grows:
 *
 * <ol>
 *   <li>function + affiliate + clause type + turn position</li>
 *   <li>function + affiliate + clause type</li>
 *   <li>function + affiliate</li>
 *   <li>function</li>
 *   <li>neighbouring Function values on the NEUROGES polar axis</li>
 *   <li>affiliate, pooled across functions</li>
 *   <li>the hand-written prior ({@link PlacementPrior})</li>
 *   <li>uniform over the slots this sentence offers</li>
 * </ol>
 *
 * <p>Level 5 is the one that earns its keep on a small corpus. The Function category is polar — its
 * values run on a stated axis from emotional to conventionalised — so a value with no observations
 * can borrow from its neighbours, which is a far better guess than falling straight to the prior.
 *
 * <p>Interpolation is Witten-Bell: a level with {@code n} relevant observations claims
 * {@code n / (n + K)} of the remaining probability mass and passes the rest down. So one observation
 * nudges, twenty dominate, and no level can silence the ones below it.
 *
 * <p>Counts are always restricted to the slots the sentence actually offers before the mass is
 * computed. Otherwise a model that had learned "after-subject" would spend its confidence on a slot
 * that does not exist in a subjectless sentence and leave the real candidates starved.
 *
 * <p>Not thread-safe; hold one instance per project and confine it to the request thread.
 */
public final class PlacementModel {

    /** Schema version of {@code behavior-placement.json}. */
    public static final int MODEL_VERSION = 1;

    /**
     * Witten-Bell smoothing constant. At K=5 a level needs five observations to claim half the
     * remaining mass. Chosen to be sceptical: with a corpus this small, a single observation should
     * inform a suggestion, never determine it.
     */
    private static final double K = 5.0;

    /**
     * Weighted counts. Doubles rather than integers because an accepted suggestion counts for less
     * than something the author wrote unprompted — see {@link #observe(PlacementContext, String,
     * double)}. The weights, not the raw tallies, are what the distribution is built from.
     */
    private final Map<String, Map<String, Double>> mCounts = new TreeMap<>();
    private int mObservations;
    private int mSnapped;
    private double mWeightTotal;

    public PlacementModel() {
    }

    public static PlacementModel empty() {
        return new PlacementModel();
    }

    // ------------------------------------------------------------------ observe

    /**
     * Record one authored placement.
     *
     * @param slot the anchor slot chosen; {@code null} or unknown slots are ignored rather than
     *             stored, so a mid-phrase placement that no slot describes cannot silently become
     *             evidence for whichever slot happened to be nearest. Callers that wish to snap
     *             should snap explicitly and report it via {@link #observeSnapped}.
     */
    public void observe(final PlacementContext context, final String slot) {
        observe(context, slot, 1.0);
    }

    /**
     * Record a placement with an explicit weight — the feedback-loop guard of plan 4.3.
     *
     * <p>A placement the author wrote unprompted is worth 1.0. One they accepted from a suggestion is
     * worth less, because it is only weak evidence about the author's own preference: accepting a
     * plausible default is not the same act as choosing a position. Counting the two alike would let
     * the model confirm itself — it suggests a slot, the author accepts, the slot's count rises, it
     * suggests it more strongly — until the model is measuring its own past output rather than the
     * author. The caller supplies the weight; the policy lives with the caller that knows the origin.
     *
     * @param weight strictly positive; a non-positive weight is ignored rather than stored, since a
     *               zero-weight observation would inflate the observation count while contributing
     *               nothing to the distribution.
     */
    public void observe(final PlacementContext context, final String slot, final double weight) {
        if (context == null || slot == null || !AnchorSlots.isKnown(slot) || weight <= 0.0) {
            return;
        }
        for (final String key : levelKeys(context)) {
            mCounts.computeIfAbsent(key, k -> new TreeMap<>()).merge(slot, weight, Double::sum);
        }
        mObservations += 1;
        mWeightTotal += weight;
    }

    /**
     * Record a placement that did not sit on any offered slot and was snapped to the nearest one.
     *
     * <p>Counted separately as well as observed. 23% of placements in the EmmaAgent corpus are
     * mid-phrase, and a model that silently absorbed them would report a confidence it has not
     * earned. The tally makes the approximation visible in {@code behavior-placement.json}.
     */
    public void observeSnapped(final PlacementContext context, final String slot) {
        observeSnapped(context, slot, 1.0);
    }

    public void observeSnapped(final PlacementContext context, final String slot, final double weight) {
        observe(context, slot, weight);
        if (slot != null && AnchorSlots.isKnown(slot)) {
            mSnapped += 1;
        }
    }

    private static List<String> levelKeys(final PlacementContext context) {
        return List.of(
                context.keyFull(),
                context.keyFunctionClause(),
                context.keyFunctionAffiliate(),
                context.keyFunction(),
                context.keyAffiliate());
    }

    // ------------------------------------------------------------------ suggest

    /**
     * Rank the slots this sentence offers, best first.
     *
     * @param offered the anchor slots the semantic analysis produced for this sentence. An empty
     *                list yields no suggestions: with nothing to choose between, inventing a
     *                position would be worse than staying silent.
     */
    public List<PlacementSuggestion> suggest(
            final PlacementContext context, final List<String> offered, final int limit) {
        if (context == null || offered == null || offered.isEmpty()) {
            return List.of();
        }
        final List<String> candidates = new ArrayList<>();
        for (final String slot : offered) {
            if (slot != null && !candidates.contains(slot)) {
                candidates.add(slot);
            }
        }
        if (candidates.isEmpty()) {
            return List.of();
        }

        final Map<String, Double> scores = new LinkedHashMap<>();
        final Map<String, PlacementSuggestion.Basis> basis = new LinkedHashMap<>();
        final Map<String, Double> support = new LinkedHashMap<>();
        final Map<String, Double> bestShare = new LinkedHashMap<>();
        double remaining = 1.0;

        for (final Level level : levels(context)) {
            if (remaining <= 1.0e-9) {
                break;
            }
            final Map<String, Double> counts = residualCounts(level.keys, level.subtractKeys, candidates);
            double total = 0.0;
            for (final double value : counts.values()) {
                total += value;
            }
            if (total <= 1.0e-9) {
                continue;
            }
            final double lambda = remaining * (total / (total + K));
            for (final Map.Entry<String, Double> entry : counts.entrySet()) {
                final double share = lambda * (entry.getValue() / total);
                accumulate(scores, basis, support, bestShare,
                        entry.getKey(), share, level.basis, entry.getValue());
            }
            remaining -= lambda;
        }

        if (remaining > 1.0e-9) {
            final Map<String, Double> prior = PlacementPrior.distribution(context.getAffiliate(), candidates);
            if (!prior.isEmpty()) {
                for (final Map.Entry<String, Double> entry : prior.entrySet()) {
                    accumulate(scores, basis, support, bestShare,
                            entry.getKey(), remaining * entry.getValue(), PlacementSuggestion.Basis.PRIOR, 0.0);
                }
                remaining = 0.0;
            }
        }
        if (remaining > 1.0e-9) {
            final double share = remaining / candidates.size();
            for (final String slot : candidates) {
                accumulate(scores, basis, support, bestShare,
                        slot, share, PlacementSuggestion.Basis.UNIFORM, 0.0);
            }
        }

        final List<PlacementSuggestion> out = new ArrayList<>();
        for (final Map.Entry<String, Double> entry : scores.entrySet()) {
            out.add(new PlacementSuggestion(
                    entry.getKey(),
                    entry.getValue(),
                    basis.getOrDefault(entry.getKey(), PlacementSuggestion.Basis.UNIFORM),
                    support.getOrDefault(entry.getKey(), 0.0)));
        }
        // Ties broken by the extractor's slot priority, so the model and the corpus agree about which
        // of two equally-scored slots is the more specific description of a position.
        out.sort(Comparator
                .comparingDouble(PlacementSuggestion::getScore).reversed()
                .thenComparingInt(s -> AnchorSlots.rankOf(s.getSlot())));
        if (limit > 0 && out.size() > limit) {
            return List.copyOf(out.subList(0, limit));
        }
        return List.copyOf(out);
    }

    private static void accumulate(
            final Map<String, Double> scores,
            final Map<String, PlacementSuggestion.Basis> basis,
            final Map<String, Double> support,
            final Map<String, Double> bestShare,
            final String slot,
            final double share,
            final PlacementSuggestion.Basis levelBasis,
            final double levelSupport) {
        scores.merge(slot, share, Double::sum);
        // The reported basis is whichever level contributed most to this slot, not the first that
        // touched it — otherwise a specific level with one observation would take credit for a score
        // that a broader level actually built.
        if (share > bestShare.getOrDefault(slot, 0.0)) {
            bestShare.put(slot, share);
            basis.put(slot, levelBasis);
            support.put(slot, levelSupport);
        }
    }

    /**
     * One back-off level: the count keys it reads, and the keys whose events it must not count again.
     *
     * <p>The broader levels are supersets of the narrower ones — every event stored under
     * {@code f=X|a=Y|c=main|t=ONLY} is also stored under {@code f=X|a=Y}. Interpolating them naively
     * counts the same event once per level, so a single observation compounded across five levels
     * reached ~0.6 confidence, which is not evidence, it is arithmetic. Each level therefore
     * subtracts its nested child and contributes only the events the narrower level did not already
     * explain.
     */
    private static final class Level {
        private final List<String> keys;
        private final List<String> subtractKeys;
        private final PlacementSuggestion.Basis basis;

        private Level(final List<String> keys, final List<String> subtractKeys,
                      final PlacementSuggestion.Basis basis) {
            this.keys = keys;
            this.subtractKeys = subtractKeys;
            this.basis = basis;
        }
    }

    private static List<Level> levels(final PlacementContext context) {
        final List<String> full = List.of(context.keyFull());
        final List<String> functionClause = List.of(context.keyFunctionClause());
        final List<String> functionAffiliate = List.of(context.keyFunctionAffiliate());
        final List<String> function = List.of(context.keyFunction());
        final List<String> affiliate = List.of(context.keyAffiliate());

        final List<Level> out = new ArrayList<>();
        out.add(new Level(full, List.of(), PlacementSuggestion.Basis.EXACT_CONTEXT));
        out.add(new Level(functionClause, full, PlacementSuggestion.Basis.FUNCTION_CLAUSE));
        out.add(new Level(functionAffiliate, functionClause, PlacementSuggestion.Basis.FUNCTION_AFFILIATE));
        // "same function, any affiliate" and "same affiliate, any function" both contain the
        // function+affiliate events, and each other's only via that intersection. Subtracting it from
        // both leaves two disjoint pools: other affiliates of this function, and other functions with
        // this affiliate.
        out.add(new Level(function, functionAffiliate, PlacementSuggestion.Basis.FUNCTION));
        final List<String> axisKeys = axisNeighbourKeys(context);
        if (!axisKeys.isEmpty()) {
            // Neighbouring Function values are disjoint from every level above, which all condition on
            // this function, so nothing is subtracted here.
            out.add(new Level(axisKeys, List.of(), PlacementSuggestion.Basis.AXIS_NEIGHBOUR));
        }
        out.add(new Level(affiliate, functionAffiliate, PlacementSuggestion.Basis.AFFILIATE));
        return out;
    }

    /**
     * Keys of the Function values adjacent to this one on the NEUROGES polar axis.
     *
     * <p>This is the level the plan singles out as what makes a corpus of a few dozen examples
     * usable. Function is an ordered category, so "emphasis" with no observations can borrow from
     * "emotion/attitude" and "egocentric deictic" either side of it.
     */
    private static List<String> axisNeighbourKeys(final PlacementContext context) {
        if (context.getFunction() == null) {
            return List.of();
        }
        final List<String> neighbours;
        try {
            neighbours = BehaviorTaxonomy.getDefault().neighboursOnAxis("function", context.getFunction());
        } catch (final RuntimeException exc) {
            // The taxonomy is a classpath resource; if it cannot be read the model still works, just
            // without axis back-off. Failing a suggestion over this would be disproportionate.
            return List.of();
        }
        if (neighbours == null || neighbours.isEmpty()) {
            return List.of();
        }
        final List<String> keys = new ArrayList<>();
        for (final String neighbour : neighbours) {
            keys.add(context.withFunction(neighbour).keyFunction());
        }
        return keys;
    }

    /**
     * Counts at this level minus those already explained by the nested level below it, floored at
     * zero so a non-nested {@code subtractKeys} can never drive a pool negative.
     */
    private Map<String, Double> residualCounts(
            final List<String> keys, final List<String> subtractKeys, final List<String> candidates) {
        final Map<String, Double> counts = pooledCounts(keys, candidates);
        if (subtractKeys.isEmpty()) {
            return counts;
        }
        final Map<String, Double> consumed = pooledCounts(subtractKeys, candidates);
        final Map<String, Double> out = new LinkedHashMap<>();
        for (final Map.Entry<String, Double> entry : counts.entrySet()) {
            final double residual = entry.getValue() - consumed.getOrDefault(entry.getKey(), 0.0);
            if (residual > 1.0e-9) {
                out.put(entry.getKey(), residual);
            }
        }
        return out;
    }

    private Map<String, Double> pooledCounts(final List<String> keys, final List<String> candidates) {
        final Map<String, Double> out = new LinkedHashMap<>();
        for (final String key : keys) {
            final Map<String, Double> counts = mCounts.get(key);
            if (counts == null) {
                continue;
            }
            for (final Map.Entry<String, Double> entry : counts.entrySet()) {
                if (candidates.contains(entry.getKey())) {
                    out.merge(entry.getKey(), entry.getValue(), Double::sum);
                }
            }
        }
        return out;
    }

    // ------------------------------------------------------------------ state

    public int getObservationCount() {
        return mObservations;
    }

    /** How many observations were snapped from a mid-phrase position to the nearest slot. */
    public int getSnappedCount() {
        return mSnapped;
    }

    /** Summed observation weights. Below the observation count when discounted evidence is present. */
    public double getWeightTotal() {
        return mWeightTotal;
    }

    public boolean isEmpty() {
        return mObservations == 0;
    }

    // ------------------------------------------------------------------ persistence

    public JSONObject toJson() {
        final JSONObject counts = new JSONObject();
        for (final Map.Entry<String, Map<String, Double>> entry : mCounts.entrySet()) {
            final JSONObject perSlot = new JSONObject();
            for (final Map.Entry<String, Double> slot : entry.getValue().entrySet()) {
                perSlot.put(slot.getKey(), Math.round(slot.getValue() * 1000.0) / 1000.0);
            }
            counts.put(entry.getKey(), perSlot);
        }
        return new JSONObject()
                .put("version", MODEL_VERSION)
                .put("observations", mObservations)
                .put("weightTotal", Math.round(mWeightTotal * 1000.0) / 1000.0)
                .put("snapped", mSnapped)
                .put("smoothingK", K)
                .put("counts", counts);
    }

    public static PlacementModel fromJson(final JSONObject json) {
        final PlacementModel model = new PlacementModel();
        if (json == null) {
            return model;
        }
        model.mObservations = json.optInt("observations", 0);
        model.mWeightTotal = json.optDouble("weightTotal", model.mObservations);
        model.mSnapped = json.optInt("snapped", 0);
        final JSONObject counts = json.optJSONObject("counts");
        if (counts == null) {
            return model;
        }
        for (final String key : counts.keySet()) {
            final JSONObject slots = counts.optJSONObject(key);
            if (slots == null) {
                continue;
            }
            final Map<String, Double> perSlot = new TreeMap<>();
            for (final String slot : slots.keySet()) {
                final double value = slots.optDouble(slot, 0.0);
                if (value > 0.0) {
                    perSlot.put(slot, value);
                }
            }
            if (!perSlot.isEmpty()) {
                model.mCounts.put(key, perSlot);
            }
        }
        return model;
    }

    @Override
    public String toString() {
        return "PlacementModel[observations=" + mObservations
                + ", snapped=" + mSnapped + ", keys=" + mCounts.size() + "]";
    }
}
