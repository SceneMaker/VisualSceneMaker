package de.dfki.vsm.model.behavior.placement;

import java.util.List;

/**
 * The structural label space for behavior-command placement.
 *
 * <p>These are the positions the semantic analysis offers inside a sentence — constituent
 * boundaries, not token offsets. A slot label is what makes a placement generalisable: "after the
 * subject" transfers between sentences, "at token 4" does not.
 *
 * <p>The order is the tie-break priority used when several slots fall at the same token, most
 * specific first. It is deliberately identical to the extractor's, so a model trained on the corpus
 * ranks slots the same way the corpus labelled them.
 */
public final class AnchorSlots {

    /** Ordered most-specific first; also the tie-break order when slots share a token index. */
    public static final List<String> PRIORITY = List.of(
            "before-object", "after-object",
            "before-subject", "after-subject",
            "before-predicate", "after-predicate",
            "after-address",
            "before-verb", "after-verb",
            "clause-initial",
            "before-final-punct",
            "utterance-initial", "utterance-final");

    private AnchorSlots() {
    }

    /**
     * Rank of a slot in {@link #PRIORITY}; unknown slots sort last rather than throwing, so a corpus
     * written by a newer extractor still loads.
     */
    public static int rankOf(final String slot) {
        final int index = PRIORITY.indexOf(slot);
        return index < 0 ? PRIORITY.size() : index;
    }

    public static boolean isKnown(final String slot) {
        return PRIORITY.contains(slot);
    }
}
