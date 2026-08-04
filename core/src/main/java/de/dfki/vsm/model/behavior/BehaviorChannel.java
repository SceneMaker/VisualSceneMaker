package de.dfki.vsm.model.behavior;

import org.json.JSONObject;

/**
 * A channel a behavior command drives, from {@code behavior-taxonomy.json}.
 *
 * <p>NEUROGES&reg; codes four body parts — upper limbs, lower limbs, head, trunk. Gaze and facial
 * expression appear <em>inside</em> value definitions as movement criteria but are never coded units,
 * and stage effects (backdrop, camera) are not behavior at all. {@link #isInNeurogesScope()} is
 * therefore three-valued:</p>
 *
 * <ul>
 *   <li>{@code TRUE} — codable body movement</li>
 *   <li>{@code FALSE} — outside the coding system (face, gaze, stage, control)</li>
 *   <li>{@code null} — undetermined; only video coding of the asset can settle it. Distinct from
 *       FALSE: such a command may well turn out to be codable.</li>
 * </ul>
 *
 * @author Patrick Gebhard
 */
public final class BehaviorChannel {

    private final String mName;
    private final String mBodyPart;
    private final Boolean mInNeurogesScope;
    private final String mNote;

    public BehaviorChannel(
            final String name,
            final String bodyPart,
            final Boolean inNeurogesScope,
            final String note) {
        mName = name;
        mBodyPart = bodyPart;
        mInNeurogesScope = inNeurogesScope;
        mNote = note;
    }

    public final String getName() {
        return mName;
    }

    /** One of the four NEUROGES body parts, or {@code null} for channels outside the system. */
    public final String getBodyPart() {
        return mBodyPart;
    }

    /** Three-valued, see the class comment. Never unbox this without a null check. */
    public final Boolean isInNeurogesScope() {
        return mInNeurogesScope;
    }

    /** True only when scope is explicitly undetermined — i.e. pending video coding. */
    public final boolean isScopeUndetermined() {
        return mInNeurogesScope == null;
    }

    public final String getNote() {
        return mNote;
    }

    public static BehaviorChannel fromJson(final JSONObject json) {
        return new BehaviorChannel(
                json.optString("name", null),
                json.isNull("bodyPart") ? null : json.optString("bodyPart", null),
                json.isNull("inNeurogesScope") ? null : Boolean.valueOf(json.optBoolean("inNeurogesScope")),
                json.isNull("note") ? null : json.optString("note", null));
    }

    @Override
    public String toString() {
        return "BehaviorChannel{" + mName + ", inNeurogesScope=" + mInNeurogesScope + "}";
    }
}
