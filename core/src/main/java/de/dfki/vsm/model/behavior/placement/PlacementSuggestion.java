package de.dfki.vsm.model.behavior.placement;

import org.json.JSONObject;

/**
 * One ranked anchor slot, with the evidence level that produced most of its score.
 *
 * <p>{@code basis} exists so a suggestion is answerable. "after-subject, 0.62, from 14 observations
 * of emotion/attitude in this project" is a claim an author can judge; a bare 0.62 is not. It also
 * makes it visible when a suggestion rests entirely on the hand-written prior, which is the normal
 * state of a new project and should not be mistaken for evidence.
 */
public final class PlacementSuggestion {

    /** Which back-off level contributed the largest share of the score. */
    public enum Basis {
        /** Same function, clause type and turn position, in this project. */
        EXACT_CONTEXT,
        /** Same function and clause type. */
        FUNCTION_CLAUSE,
        /** Same function and affiliate. */
        FUNCTION_AFFILIATE,
        /** Same NEUROGES Function, any clause. */
        FUNCTION,
        /** A neighbouring Function value on the NEUROGES polar axis. */
        AXIS_NEIGHBOUR,
        /** Same affiliate, pooled across functions. */
        AFFILIATE,
        /** The hand-written prior — no observation of this kind yet. */
        PRIOR,
        /** Nothing applied; mass spread evenly over the offered slots. */
        UNIFORM
    }

    private final String mSlot;
    private final double mScore;
    private final Basis mBasis;
    private final int mSupport;

    public PlacementSuggestion(final String slot, final double score, final Basis basis, final int support) {
        mSlot = slot;
        mScore = score;
        mBasis = basis;
        mSupport = support;
    }

    public String getSlot() {
        return mSlot;
    }

    /** Probability mass assigned to this slot, over the slots the sentence actually offers. */
    public double getScore() {
        return mScore;
    }

    public Basis getBasis() {
        return mBasis;
    }

    /** Observations behind the dominant level; 0 when the basis is the prior. */
    public int getSupport() {
        return mSupport;
    }

    /** True when nothing was learned for this context and the prior alone decided it. */
    public boolean isPriorOnly() {
        return mBasis == Basis.PRIOR || mBasis == Basis.UNIFORM;
    }

    public JSONObject toJson() {
        return new JSONObject()
                .put("slot", mSlot)
                .put("score", Math.round(mScore * 10000.0) / 10000.0)
                .put("basis", mBasis.name().toLowerCase(java.util.Locale.ROOT).replace('_', '-'))
                .put("support", mSupport)
                .put("priorOnly", isPriorOnly());
    }

    @Override
    public String toString() {
        return mSlot + "=" + String.format(java.util.Locale.ROOT, "%.3f", mScore)
                + "(" + mBasis + "/" + mSupport + ")";
    }
}
