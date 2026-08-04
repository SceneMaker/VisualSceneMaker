package de.dfki.vsm.model.behavior;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * One NEUROGES&reg; category from {@code behavior-taxonomy.json} — either one of the seven core
 * categories (Activation, Structure, Focus, Contact, Formal Relation, Function, Type) or one of the
 * seven supplementary ones.
 *
 * <p>The value list order is <em>significant</em>: for a {@link #isPolar() polar} category it is the
 * horizontal axis of the coding system, along which {@link #neighboursOf(String)} walks. That is
 * what lets a placement model back off to an adjacent value instead of collapsing to a generic
 * prior, so the order must never be sorted or de-duplicated.</p>
 *
 * @author Patrick Gebhard
 */
public final class TaxonomyCategory {

    private final String mName;
    private final String mModule;
    private final int mStep;
    private final String mDefinition;
    private final List<String> mValues;
    private final List<String> mRestPoseValues;
    private final List<String> mSpecialValues;
    private final boolean mPolar;
    private final String mAxis;
    private final Map<String, List<String>> mMainGroups;
    private final Map<String, List<String>> mTypesByFunction;
    private final Map<String, String> mReliability;
    private final List<String> mAppliesTo;
    private final String mStatus;

    public TaxonomyCategory(
            final String name,
            final String module,
            final int step,
            final String definition,
            final List<String> values,
            final List<String> restPoseValues,
            final List<String> specialValues,
            final boolean polar,
            final String axis,
            final Map<String, List<String>> mainGroups,
            final Map<String, List<String>> typesByFunction,
            final Map<String, String> reliability,
            final List<String> appliesTo,
            final String status) {
        mName = name;
        mModule = module;
        mStep = step;
        mDefinition = definition;
        mValues = (values == null) ? Collections.emptyList() : Collections.unmodifiableList(values);
        mRestPoseValues = (restPoseValues == null)
                ? Collections.emptyList() : Collections.unmodifiableList(restPoseValues);
        mSpecialValues = (specialValues == null)
                ? Collections.emptyList() : Collections.unmodifiableList(specialValues);
        mPolar = polar;
        mAxis = axis;
        mMainGroups = (mainGroups == null)
                ? Collections.emptyMap() : Collections.unmodifiableMap(mainGroups);
        mTypesByFunction = (typesByFunction == null)
                ? Collections.emptyMap() : Collections.unmodifiableMap(typesByFunction);
        mReliability = (reliability == null)
                ? Collections.emptyMap() : Collections.unmodifiableMap(reliability);
        mAppliesTo = (appliesTo == null) ? Collections.emptyList() : Collections.unmodifiableList(appliesTo);
        mStatus = status;
    }

    public final String getName() {
        return mName;
    }

    /** NEUROGES module this category belongs to ("I", "II", "III"), or {@code null} for
     *  supplementary categories. Provenance only — see doc/behavior-taxonomy-neuroges.md §2. */
    public final String getModule() {
        return mModule;
    }

    /** Step in the complete algorithmic analysis (1..7), or 0 for supplementary categories. */
    public final int getStep() {
        return mStep;
    }

    public final String getDefinition() {
        return mDefinition;
    }

    /** The category's values, in the coding system's horizontal order. Never sort this. */
    public final List<String> getValues() {
        return mValues;
    }

    /** Values that apply to rest/pose units rather than movement units (Structure, Contact). */
    public final List<String> getRestPoseValues() {
        return mRestPoseValues;
    }

    /** Template-only escape values such as {@code prep-retract} or {@code different functions}. */
    public final List<String> getSpecialValues() {
        return mSpecialValues;
    }

    public final boolean isPolar() {
        return mPolar;
    }

    /** Prose description of what the horizontal order means, from the coding manual. */
    public final String getAxis() {
        return mAxis;
    }

    /** For the Function category: named groups of values ("egocentric gestures", …). */
    public final Map<String, List<String>> getMainGroups() {
        return mMainGroups;
    }

    /** For the Type category: the Type values available under each Function value. Type is a
     *  dependent category — the parent Function determines the legal Type values. */
    public final Map<String, List<String>> getTypesByFunction() {
        return mTypesByFunction;
    }

    /** Per-value interrater reliability as published (EasyDIAg, Lausberg &amp; Slöetjes 2016),
     *  e.g. {@code "0.43 ± 0.29"}. Absent or null-valued where the manual reports no data. */
    public final Map<String, String> getReliability() {
        return mReliability;
    }

    /** Supplementary categories only: the Function values this category is assessed for. */
    public final List<String> getAppliesTo() {
        return mAppliesTo;
    }

    /** Supplementary categories only: e.g. work-in-progress marker from the manual. */
    public final String getStatus() {
        return mStatus;
    }

    /**
     * Every value this category accepts: axis values, rest/pose values and special values — plus,
     * for a dependent category such as Type, the union of its per-parent value lists. Type declares
     * no {@code values} array of its own (the legal values depend on the parent Function), so
     * without the {@code typesByFunction} union nothing would validate against it.
     */
    public final Set<String> allValues() {
        final Set<String> all = new LinkedHashSet<>(mValues);
        all.addAll(mRestPoseValues);
        all.addAll(mSpecialValues);
        for (final List<String> dependent : mTypesByFunction.values()) {
            all.addAll(dependent);
        }
        return Collections.unmodifiableSet(all);
    }

    public final boolean accepts(final String value) {
        return value != null && allValues().contains(value);
    }

    /**
     * Position of {@code value} on the horizontal axis, or -1 if it is not an axis value.
     * Rest/pose and special values are deliberately excluded: they are not on the axis.
     */
    public final int axisIndexOf(final String value) {
        return mValues.indexOf(value);
    }

    /**
     * The values immediately adjacent to {@code value} on the horizontal axis — the principled
     * back-off step for a polar category. Returns an empty list for a non-polar category, for a
     * value that is not on the axis, or for an axis of fewer than two values.
     *
     * <p>Example: {@code focus.neighboursOf("on body")} yields
     * {@code ["within body", "on attached object"]}.</p>
     */
    public final List<String> neighboursOf(final String value) {
        if (!mPolar) {
            return Collections.emptyList();
        }
        final int index = axisIndexOf(value);
        if (index < 0) {
            return Collections.emptyList();
        }
        final List<String> neighbours = new ArrayList<>(2);
        if (index > 0) {
            neighbours.add(mValues.get(index - 1));
        }
        if (index < mValues.size() - 1) {
            neighbours.add(mValues.get(index + 1));
        }
        return Collections.unmodifiableList(neighbours);
    }

    /** Name of the main group containing {@code value}, or {@code null} if none does. */
    public final String mainGroupOf(final String value) {
        for (final Map.Entry<String, List<String>> entry : mMainGroups.entrySet()) {
            if (entry.getValue().contains(value)) {
                return entry.getKey();
            }
        }
        return null;
    }

    public static TaxonomyCategory fromJson(final JSONObject json) {
        return new TaxonomyCategory(
                json.optString("name", null),
                json.isNull("module") ? null : json.optString("module", null),
                json.optInt("step", 0),
                json.optString("definition", null),
                readStrings(json.optJSONArray("values")),
                readStrings(json.optJSONArray("restPoseValues")),
                readStrings(json.optJSONArray("specialValues")),
                json.optBoolean("polar", false),
                json.isNull("axis") ? null : json.optString("axis", null),
                readStringLists(json.optJSONObject("mainGroups")),
                readStringLists(json.optJSONObject("typesByFunction")),
                readStringMap(json.optJSONObject("reliability")),
                readStrings(json.optJSONArray("appliesTo")),
                json.isNull("status") ? null : json.optString("status", null));
    }

    static List<String> readStrings(final JSONArray json) {
        if (json == null) {
            return Collections.emptyList();
        }
        final List<String> out = new ArrayList<>(json.length());
        for (int i = 0; i < json.length(); i++) {
            out.add(json.optString(i));
        }
        return out;
    }

    private static Map<String, List<String>> readStringLists(final JSONObject json) {
        if (json == null) {
            return Collections.emptyMap();
        }
        final Map<String, List<String>> out = new LinkedHashMap<>();
        for (final String key : json.keySet()) {
            out.put(key, readStrings(json.optJSONArray(key)));
        }
        return out;
    }

    private static Map<String, String> readStringMap(final JSONObject json) {
        if (json == null) {
            return Collections.emptyMap();
        }
        final Map<String, String> out = new LinkedHashMap<>();
        for (final String key : json.keySet()) {
            // A null value means "the manual reports no data for this value" — keep the key so
            // callers can tell "no data" apart from "value not in this category".
            out.put(key, json.isNull(key) ? null : json.optString(key));
        }
        return out;
    }

    @Override
    public String toString() {
        return "TaxonomyCategory{" + mName + ", " + mValues.size() + " values, polar=" + mPolar + "}";
    }
}
