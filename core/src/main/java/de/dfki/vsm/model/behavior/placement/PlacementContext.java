package de.dfki.vsm.model.behavior.placement;

import java.util.Locale;
import java.util.Objects;

/**
 * The conditioning variables of a placement decision: what kind of behavior is being placed, and
 * into what kind of sentence.
 *
 * <p>Every field is optional. A context with nothing but an affiliate still yields a prediction from
 * the hand-written prior, which is the point — the model has to be useful at n=0, before a project
 * has any corpus of its own.
 */
public final class PlacementContext {

    /** Where in the turn the sentence sits. Authors treat the opening of a turn differently. */
    public enum TurnPosition {
        FIRST, MIDDLE, LAST, ONLY, UNKNOWN;

        public static TurnPosition of(final int sentenceIndex, final int sentenceCount) {
            if (sentenceCount <= 0 || sentenceIndex < 0) {
                return UNKNOWN;
            }
            if (sentenceCount == 1) {
                return ONLY;
            }
            if (sentenceIndex == 0) {
                return FIRST;
            }
            return sentenceIndex >= sentenceCount - 1 ? LAST : MIDDLE;
        }
    }

    private final String mFunction;
    private final String mAffiliate;
    private final String mClauseType;
    private final TurnPosition mTurnPosition;
    private final String mDialogueAct;

    public PlacementContext(
            final String function,
            final String affiliate,
            final String clauseType,
            final TurnPosition turnPosition,
            final String dialogueAct) {
        mFunction = normalize(function);
        mAffiliate = normalize(affiliate);
        mClauseType = normalize(clauseType);
        mTurnPosition = turnPosition == null ? TurnPosition.UNKNOWN : turnPosition;
        mDialogueAct = normalize(dialogueAct);
    }

    private static String normalize(final String value) {
        if (value == null) {
            return null;
        }
        final String trimmed = value.trim().toLowerCase(Locale.ROOT);
        return trimmed.isEmpty() ? null : trimmed;
    }

    public String getFunction() {
        return mFunction;
    }

    public String getAffiliate() {
        return mAffiliate;
    }

    public String getClauseType() {
        return mClauseType;
    }

    public TurnPosition getTurnPosition() {
        return mTurnPosition;
    }

    public String getDialogueAct() {
        return mDialogueAct;
    }

    /** A context with the function replaced — used to pool counts across NEUROGES axis neighbours. */
    public PlacementContext withFunction(final String function) {
        return new PlacementContext(function, mAffiliate, mClauseType, mTurnPosition, mDialogueAct);
    }

    private static String part(final String value) {
        return value == null ? "*" : value;
    }

    /**
     * Keys for each back-off level, most specific first. Kept as flat strings so the persisted model
     * is readable — someone should be able to open {@code behavior-placement.json} and see what it
     * learned, which is the whole point of choosing a count model over an opaque one.
     */
    public String keyFull() {
        return "f=" + part(mFunction) + "|a=" + part(mAffiliate)
                + "|c=" + part(mClauseType) + "|t=" + mTurnPosition;
    }

    public String keyFunctionClause() {
        return "f=" + part(mFunction) + "|a=" + part(mAffiliate) + "|c=" + part(mClauseType);
    }

    public String keyFunctionAffiliate() {
        return "f=" + part(mFunction) + "|a=" + part(mAffiliate);
    }

    public String keyFunction() {
        return "f=" + part(mFunction);
    }

    public String keyAffiliate() {
        return "a=" + part(mAffiliate);
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof PlacementContext)) {
            return false;
        }
        final PlacementContext that = (PlacementContext) other;
        return Objects.equals(mFunction, that.mFunction)
                && Objects.equals(mAffiliate, that.mAffiliate)
                && Objects.equals(mClauseType, that.mClauseType)
                && mTurnPosition == that.mTurnPosition
                && Objects.equals(mDialogueAct, that.mDialogueAct);
    }

    @Override
    public int hashCode() {
        return Objects.hash(mFunction, mAffiliate, mClauseType, mTurnPosition, mDialogueAct);
    }

    @Override
    public String toString() {
        return "PlacementContext[" + keyFull() + "]";
    }
}
