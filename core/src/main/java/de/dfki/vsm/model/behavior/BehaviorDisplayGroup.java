package de.dfki.vsm.model.behavior;

import org.json.JSONObject;

import java.util.Collections;
import java.util.List;

/**
 * An author-facing display group: the reduction of NEUROGES&reg;' 11 Function values and 24 Type
 * values to a handful of column headings a non-programmer can scan.
 *
 * <p>Display groups are <b>VSM-derived and carry no NEUROGES authority</b> — they exist because the
 * full vocabulary is too large for an authoring UI. They live in {@code behavior-taxonomy.json}
 * rather than in the Web UI so the reduction stays reviewable alongside the classification it
 * reduces (see {@code doc/behavior-taxonomy-neuroges.md} §8.4).</p>
 *
 * <p>A group matches a command either by its NEUROGES Function value or by its channel; Function
 * takes precedence. See {@link BehaviorTaxonomy#displayGroupOf(BehaviorTag)}.</p>
 *
 * @author Patrick Gebhard
 */
public final class BehaviorDisplayGroup {

    private final String mId;
    private final String mLabel;
    private final boolean mSiaVisible;
    private final List<String> mFunctions;
    private final List<String> mChannels;
    private final String mNote;

    public BehaviorDisplayGroup(
            final String id,
            final String label,
            final boolean siaVisible,
            final List<String> functions,
            final List<String> channels,
            final String note) {
        mId = id;
        mLabel = label;
        mSiaVisible = siaVisible;
        mFunctions = (functions == null) ? Collections.emptyList() : Collections.unmodifiableList(functions);
        mChannels = (channels == null) ? Collections.emptyList() : Collections.unmodifiableList(channels);
        mNote = note;
    }

    /** Stable identifier, used as a UI column key. */
    public final String getId() {
        return mId;
    }

    public final String getLabel() {
        return mLabel;
    }

    /** Whether the SIA character-preview panel shows this group. */
    public final boolean isSiaVisible() {
        return mSiaVisible;
    }

    /** NEUROGES Function values this group collapses. */
    public final List<String> getFunctions() {
        return mFunctions;
    }

    /** Channels this group collects, for commands with no resolved Function. */
    public final List<String> getChannels() {
        return mChannels;
    }

    public final String getNote() {
        return mNote;
    }

    /** True if the tag's resolved Function belongs to this group. */
    public final boolean matchesFunction(final BehaviorTag tag) {
        final String function = (tag == null) ? null : tag.getFunction();
        return function != null && mFunctions.contains(function);
    }

    /** True if the tag's channel belongs to this group. */
    public final boolean matchesChannel(final BehaviorTag tag) {
        final String channel = (tag == null) ? null : tag.getChannel();
        return channel != null && mChannels.contains(channel);
    }

    public static BehaviorDisplayGroup fromJson(final JSONObject json) {
        return new BehaviorDisplayGroup(
                json.optString("id", null),
                json.optString("label", null),
                json.optBoolean("siaVisible", false),
                TaxonomyCategory.readStrings(json.optJSONArray("functions")),
                TaxonomyCategory.readStrings(json.optJSONArray("channels")),
                json.isNull("note") ? null : json.optString("note", null));
    }

    @Override
    public String toString() {
        return "BehaviorDisplayGroup{" + mId + ", siaVisible=" + mSiaVisible + "}";
    }
}
