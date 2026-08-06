package de.dfki.vsm.model.behavior.placement;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The hand-written prior: where a behavior goes when the project has never shown us one like it.
 *
 * <p>It keys on the taxonomy's {@code affiliate} field rather than on NEUROGES Function. That is
 * deliberate and is what {@code vsmFields.affiliate} in {@code behavior-taxonomy.json} says it is
 * for — "the bridge to the anchor slots of the placement service". Affiliate states what the
 * behavior attaches to semantically, which is exactly the question a slot answers; Function states
 * what the movement *is*, which constrains placement only indirectly. Affiliate is also far better
 * populated: 58 of 66 tagged commands carry one, against 24 with a Function.
 *
 * <p>Function still matters — it is the strongest empirical conditioning variable, and the axis
 * back-off in {@link PlacementModel} runs over it. The division of labour is that observations are
 * indexed by Function and the fallback is indexed by affiliate.
 */
public final class PlacementPrior {

    /**
     * Ranked slots per affiliate, best first. Ranked rather than weighted: the exact numbers would be
     * invented, whereas the ordering is defensible from what each affiliate means.
     */
    private static final Map<String, List<String>> BY_AFFILIATE = buildPrior();

    /** Used when the affiliate is unknown or absent. */
    private static final List<String> DEFAULT_RANKING = List.of(
            "utterance-initial", "after-subject", "before-predicate");

    private static Map<String, List<String>> buildPrior() {
        final Map<String, List<String>> out = new LinkedHashMap<>();
        // A deictic or depicting gesture lands on the thing being referred to. The object is the
        // usual referent; the subject is the fallback for utterances whose referent is the speaker
        // or addressee.
        out.put("referent", List.of("before-object", "after-object", "before-subject", "after-subject"));
        // German puts new information late, so the rheme sits around the predicate and the clause-final
        // verb. This is a structural approximation of "new information", not a theme/rheme analysis:
        // the themeRheme layer is LLM-only and often absent.
        out.put("rheme", List.of("before-predicate", "after-predicate", "before-verb", "before-final-punct"));
        // Without a prosodic model we cannot find the accented word, so we approximate it by the
        // rheme region where the nuclear accent usually falls. Flagged in the plan as the honest
        // long-term answer for mid-phrase placements.
        out.put("accented-word", List.of("before-predicate", "after-predicate", "before-object"));
        // Pauses and stage changes attach to the clause as a whole, so they belong at its edges.
        out.put("clause", List.of("clause-initial", "before-final-punct", "utterance-initial"));
        out.put("whole-utterance", List.of("utterance-initial", "utterance-final"));
        // "none" means the behavior is not speech-affiliated at all — a backdrop change, a control
        // action. Such commands are normally not co-speech and so never reach the model, but if one
        // does, the least disruptive position is before the speech starts.
        out.put("none", List.of("utterance-initial"));
        return out;
    }

    private PlacementPrior() {
    }

    /** Ranked slots for an affiliate, best first; never empty. */
    public static List<String> ranking(final String affiliate) {
        if (affiliate == null) {
            return DEFAULT_RANKING;
        }
        return BY_AFFILIATE.getOrDefault(affiliate, DEFAULT_RANKING);
    }

    public static boolean covers(final String affiliate) {
        return affiliate != null && BY_AFFILIATE.containsKey(affiliate);
    }

    /**
     * The prior as a distribution over {@code offered}, geometrically decaying down the ranking.
     *
     * <p>Slots the sentence does not offer are skipped rather than dropped silently — a ranking whose
     * first choice is absent falls through to its second. If none of the ranked slots is offered the
     * result is empty and the caller spreads the remaining mass uniformly.
     */
    public static Map<String, Double> distribution(final String affiliate, final List<String> offered) {
        final Map<String, Double> out = new LinkedHashMap<>();
        double weight = 1.0;
        double total = 0.0;
        for (final String slot : ranking(affiliate)) {
            if (!offered.contains(slot)) {
                continue;
            }
            out.put(slot, weight);
            total += weight;
            weight *= 0.5;
        }
        if (total <= 0.0) {
            return out;
        }
        for (final Map.Entry<String, Double> entry : out.entrySet()) {
            entry.setValue(entry.getValue() / total);
        }
        return out;
    }
}
