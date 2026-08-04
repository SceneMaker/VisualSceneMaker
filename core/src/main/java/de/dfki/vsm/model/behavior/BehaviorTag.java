package de.dfki.vsm.model.behavior;

import org.json.JSONObject;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * The NEUROGES&reg; classification of one plugin behavior command, from the {@code commands} array of
 * {@code behavior-taxonomy.json}.
 *
 * <h2>Three distinct kinds of "no value"</h2>
 * These must not be collapsed — the taxonomy relies on the distinction:
 * <ul>
 *   <li><b>No NEUROGES unit at all</b> ({@code "neuroges": null}) — gaze, face-only, stage, control.
 *       {@link #hasNeurogesUnit()} is false.</li>
 *   <li><b>Category not applicable</b> (key absent) — e.g. a rest/pose command has no {@code function}
 *       key, because NEUROGES assesses Function only for phasic and repetitive units.
 *       {@link #isApplicable(String)} is false.</li>
 *   <li><b>Undetermined</b> (key present, value null) — pending video coding.
 *       {@link #isUndetermined(String)} is true.</li>
 * </ul>
 *
 * <h2>Authority</h2>
 * Values under {@code neuroges} and {@code supplementary} use verbatim NEUROGES value names and are
 * authoritative vocabulary. Whether <em>this command</em> deserves its tag is a separate question,
 * answered by {@link #getEvidence()}: only {@link Evidence#VIDEO_CODED} is NEUROGES-grade. See
 * {@link #isNeurogesGrade()}.
 *
 * @author Patrick Gebhard
 */
public final class BehaviorTag {

    /** How a tag was obtained. Only {@link #VIDEO_CODED} may be reported as NEUROGES-grade. */
    public enum Evidence {
        VIDEO_CODED("video-coded"),
        DECLARED_BY_VENDOR("declared-by-vendor"),
        INFERRED_FROM_NAME("inferred-from-name"),
        UNKNOWN("unknown");

        private final String mWire;

        Evidence(final String wire) {
            mWire = wire;
        }

        public String getWireName() {
            return mWire;
        }

        public static Evidence fromWire(final String wire) {
            if (wire != null) {
                for (final Evidence candidate : values()) {
                    if (candidate.mWire.equals(wire)) {
                        return candidate;
                    }
                }
            }
            return UNKNOWN;
        }
    }

    private final String mPlugin;
    private final String mCommand;
    private final String mChannel;
    private final boolean mHasNeurogesUnit;
    private final Map<String, String> mNeuroges;
    private final Set<String> mUndetermined;
    private final Map<String, String> mSupplementary;
    private final boolean mCoSpeech;
    private final String mAffiliate;
    private final Evidence mEvidence;
    private final double mConfidence;
    private final String mNote;

    BehaviorTag(
            final String plugin,
            final String command,
            final String channel,
            final boolean hasNeurogesUnit,
            final Map<String, String> neuroges,
            final Set<String> undetermined,
            final Map<String, String> supplementary,
            final boolean coSpeech,
            final String affiliate,
            final Evidence evidence,
            final double confidence,
            final String note) {
        mPlugin = plugin;
        mCommand = command;
        mChannel = channel;
        mHasNeurogesUnit = hasNeurogesUnit;
        mNeuroges = (neuroges == null) ? Collections.emptyMap() : Collections.unmodifiableMap(neuroges);
        mUndetermined = (undetermined == null) ? Collections.emptySet() : Collections.unmodifiableSet(undetermined);
        mSupplementary = (supplementary == null)
                ? Collections.emptyMap() : Collections.unmodifiableMap(supplementary);
        mCoSpeech = coSpeech;
        mAffiliate = affiliate;
        mEvidence = (evidence == null) ? Evidence.UNKNOWN : evidence;
        mConfidence = confidence;
        mNote = note;
    }

    public final String getPlugin() {
        return mPlugin;
    }

    public final String getCommand() {
        return mCommand;
    }

    public final String getChannel() {
        return mChannel;
    }

    /** False when the command drives nothing NEUROGES codes (gaze, face-only, stage, control). */
    public final boolean hasNeurogesUnit() {
        return mHasNeurogesUnit;
    }

    /** Resolved values only. A category that is present-but-undetermined is absent from this map;
     *  use {@link #isApplicable(String)} / {@link #isUndetermined(String)} to tell the cases apart. */
    public final Map<String, String> getNeuroges() {
        return mNeuroges;
    }

    public final Map<String, String> getSupplementary() {
        return mSupplementary;
    }

    /** Whether the category was asserted at all for this command — resolved or undetermined. */
    public final boolean isApplicable(final String category) {
        return mNeuroges.containsKey(category) || mUndetermined.contains(category);
    }

    /** Whether the category applies but no value has been determined yet (pending video coding). */
    public final boolean isUndetermined(final String category) {
        return mUndetermined.contains(category);
    }

    /** Resolved value for a category, or {@code null} if absent or undetermined. */
    public final String getValue(final String category) {
        return mNeuroges.get(category);
    }

    public final String getFunction() {
        return getValue("function");
    }

    public final String getType() {
        return getValue("type");
    }

    /** Whether this behavior is speech-accompanying, and therefore a candidate for placement inside
     *  an utterance. VSM-derived, not a NEUROGES field: self-regulatory actions, postures, stage
     *  effects and control commands are all false. */
    public final boolean isCoSpeech() {
        return mCoSpeech;
    }

    /** What the behavior attaches to semantically: {@code referent}, {@code rheme},
     *  {@code accented-word}, {@code clause}, {@code whole-utterance}, {@code none}, or
     *  {@code null} when undetermined. VSM-derived — the bridge to placement anchor slots. */
    public final String getAffiliate() {
        return mAffiliate;
    }

    public final Evidence getEvidence() {
        return mEvidence;
    }

    /** True only for video-coded tags. Anything else may be used for VSM-internal grouping and
     *  placement back-off, but must never be reported as a NEUROGES-grade annotation. */
    public final boolean isNeurogesGrade() {
        return mEvidence == Evidence.VIDEO_CODED;
    }

    public final double getConfidence() {
        return mConfidence;
    }

    public final String getNote() {
        return mNote;
    }

    /** Stable identity of the tagged command, {@code "plugin/command"}. */
    public final String getKey() {
        return key(mPlugin, mCommand);
    }

    static String key(final String plugin, final String command) {
        return plugin + "/" + command;
    }

    public static BehaviorTag fromJson(final JSONObject json) {
        final Map<String, String> neuroges = new LinkedHashMap<>();
        final Set<String> undetermined = new LinkedHashSet<>();
        final boolean hasUnit = !json.isNull("neuroges");
        if (hasUnit) {
            final JSONObject values = json.optJSONObject("neuroges");
            if (values != null) {
                for (final String category : values.keySet()) {
                    if (values.isNull(category)) {
                        undetermined.add(category);
                    } else {
                        neuroges.put(category, values.optString(category));
                    }
                }
            }
        }

        final Map<String, String> supplementary = new LinkedHashMap<>();
        final JSONObject supp = json.optJSONObject("supplementary");
        if (supp != null) {
            for (final String category : supp.keySet()) {
                if (!supp.isNull(category)) {
                    supplementary.put(category, supp.optString(category));
                }
            }
        }

        final JSONObject vsm = json.optJSONObject("vsm");
        final boolean coSpeech = vsm != null && vsm.optBoolean("cospeech", false);
        final String affiliate = (vsm == null || vsm.isNull("affiliate"))
                ? null : vsm.optString("affiliate", null);

        return new BehaviorTag(
                json.optString("plugin", null),
                json.optString("command", null),
                json.optString("channel", null),
                hasUnit,
                neuroges,
                undetermined,
                supplementary,
                coSpeech,
                affiliate,
                Evidence.fromWire(json.isNull("evidence") ? null : json.optString("evidence", null)),
                json.optDouble("confidence", 0.0),
                json.isNull("note") ? null : json.optString("note", null));
    }

    @Override
    public String toString() {
        return "BehaviorTag{" + getKey() + ", channel=" + mChannel
                + ", function=" + getFunction() + ", evidence=" + mEvidence.getWireName() + "}";
    }
}
