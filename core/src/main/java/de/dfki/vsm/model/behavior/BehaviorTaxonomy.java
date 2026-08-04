package de.dfki.vsm.model.behavior;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The behavior taxonomy: NEUROGES&reg; categories plus the classification of every plugin behavior
 * command. Loaded from {@code /behavior-taxonomy.json} on the classpath.
 *
 * <p>The taxonomy is the authority for behavior classification — deliberately <em>not</em>
 * {@code plugin-properties.json}, whose {@code specHash}/{@code specVersion} would churn on every
 * taxonomy revision. See {@code doc/behavior-taxonomy-neuroges.md} §5.</p>
 *
 * <p>Two things consumers most often want:</p>
 * <pre>
 *   BehaviorTaxonomy tax = BehaviorTaxonomy.getDefault();
 *
 *   // gate a placement suggestion — postures and self-regulatory actions are not co-speech
 *   if (tax.isCoSpeech("charamel-ws", "pointopenpalm")) { ... }
 *
 *   // back off along a polar axis instead of collapsing to a generic prior
 *   List&lt;String&gt; fallbacks = tax.neighboursOnAxis("focus", "on body");
 * </pre>
 *
 * <p>Java 17 and Android-safe: {@code org.json} plus the JDK only.</p>
 *
 * @author Patrick Gebhard
 */
public final class BehaviorTaxonomy {

    /** Classpath location of the bundled taxonomy. */
    public static final String RESOURCE_PATH = "/behavior-taxonomy.json";

    private static volatile BehaviorTaxonomy sDefault;

    private final String mSystem;
    private final String mSystemVersion;
    private final String mSource;
    private final String mReliabilityMeasure;
    private final List<String> mBodyParts;
    private final Map<String, TaxonomyCategory> mCategories;
    private final Map<String, TaxonomyCategory> mSupplementaryCategories;
    private final Map<String, BehaviorChannel> mChannels;
    private final List<BehaviorDisplayGroup> mDisplayGroups;
    private final Map<String, BehaviorTag> mTags;

    private BehaviorTaxonomy(
            final String system,
            final String systemVersion,
            final String source,
            final String reliabilityMeasure,
            final List<String> bodyParts,
            final Map<String, TaxonomyCategory> categories,
            final Map<String, TaxonomyCategory> supplementaryCategories,
            final Map<String, BehaviorChannel> channels,
            final List<BehaviorDisplayGroup> displayGroups,
            final Map<String, BehaviorTag> tags) {
        mSystem = system;
        mSystemVersion = systemVersion;
        mSource = source;
        mReliabilityMeasure = reliabilityMeasure;
        mBodyParts = Collections.unmodifiableList(bodyParts);
        mCategories = Collections.unmodifiableMap(categories);
        mSupplementaryCategories = Collections.unmodifiableMap(supplementaryCategories);
        mChannels = Collections.unmodifiableMap(channels);
        mDisplayGroups = Collections.unmodifiableList(displayGroups);
        mTags = Collections.unmodifiableMap(tags);
    }

    /**
     * The bundled taxonomy, parsed once and cached.
     *
     * @throws IllegalStateException if the resource is missing or unparseable — the taxonomy is
     *         shipped with core, so its absence is a packaging fault, not a runtime condition.
     */
    public static BehaviorTaxonomy getDefault() {
        BehaviorTaxonomy local = sDefault;
        if (local == null) {
            synchronized (BehaviorTaxonomy.class) {
                local = sDefault;
                if (local == null) {
                    local = loadBundled();
                    sDefault = local;
                }
            }
        }
        return local;
    }

    private static BehaviorTaxonomy loadBundled() {
        try (InputStream in = BehaviorTaxonomy.class.getResourceAsStream(RESOURCE_PATH)) {
            if (in == null) {
                throw new IllegalStateException("Behavior taxonomy resource not found: " + RESOURCE_PATH);
            }
            final String text = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            return fromJson(new JSONObject(text));
        } catch (final IOException exc) {
            throw new IllegalStateException("Cannot read behavior taxonomy " + RESOURCE_PATH, exc);
        }
    }

    public static BehaviorTaxonomy fromJson(final JSONObject json) {
        final JSONObject meta = json.optJSONObject("taxonomy");

        final Map<String, TaxonomyCategory> categories = new LinkedHashMap<>();
        final JSONArray categoryJson = json.optJSONArray("categories");
        if (categoryJson != null) {
            for (int i = 0; i < categoryJson.length(); i++) {
                final JSONObject entry = categoryJson.optJSONObject(i);
                if (entry != null) {
                    final TaxonomyCategory category = TaxonomyCategory.fromJson(entry);
                    categories.put(category.getName(), category);
                }
            }
        }

        final Map<String, TaxonomyCategory> supplementary = new LinkedHashMap<>();
        final JSONArray supplementaryJson = json.optJSONArray("supplementaryCategories");
        if (supplementaryJson != null) {
            for (int i = 0; i < supplementaryJson.length(); i++) {
                final JSONObject entry = supplementaryJson.optJSONObject(i);
                if (entry != null) {
                    final TaxonomyCategory category = TaxonomyCategory.fromJson(entry);
                    supplementary.put(category.getName(), category);
                }
            }
        }

        final Map<String, BehaviorChannel> channels = new LinkedHashMap<>();
        final JSONArray channelJson = json.optJSONArray("channels");
        if (channelJson != null) {
            for (int i = 0; i < channelJson.length(); i++) {
                final JSONObject entry = channelJson.optJSONObject(i);
                if (entry != null) {
                    final BehaviorChannel channel = BehaviorChannel.fromJson(entry);
                    channels.put(channel.getName(), channel);
                }
            }
        }

        final List<BehaviorDisplayGroup> displayGroups = new ArrayList<>();
        final JSONArray displayGroupJson = json.optJSONArray("displayGroups");
        if (displayGroupJson != null) {
            for (int i = 0; i < displayGroupJson.length(); i++) {
                final JSONObject entry = displayGroupJson.optJSONObject(i);
                if (entry != null) {
                    displayGroups.add(BehaviorDisplayGroup.fromJson(entry));
                }
            }
        }

        final Map<String, BehaviorTag> tags = new LinkedHashMap<>();
        final JSONArray commandJson = json.optJSONArray("commands");
        if (commandJson != null) {
            for (int i = 0; i < commandJson.length(); i++) {
                final JSONObject entry = commandJson.optJSONObject(i);
                if (entry != null) {
                    final BehaviorTag tag = BehaviorTag.fromJson(entry);
                    tags.put(tag.getKey(), tag);
                }
            }
        }

        return new BehaviorTaxonomy(
                meta == null ? null : meta.optString("system", null),
                meta == null ? null : meta.optString("systemVersion", null),
                meta == null ? null : meta.optString("source", null),
                meta == null ? null : meta.optString("reliabilityMeasure", null),
                TaxonomyCategory.readStrings(json.optJSONArray("bodyParts")),
                categories,
                supplementary,
                channels,
                displayGroups,
                tags);
    }

    /** The coding system, {@code "NEUROGES"}. */
    public final String getSystem() {
        return mSystem;
    }

    /** Pinned version of the coding system. A change here is a migration, never a silent re-tag. */
    public final String getSystemVersion() {
        return mSystemVersion;
    }

    public final String getSource() {
        return mSource;
    }

    public final String getReliabilityMeasure() {
        return mReliabilityMeasure;
    }

    /** The four body parts NEUROGES codes. */
    public final List<String> getBodyParts() {
        return mBodyParts;
    }

    /** The seven core categories, in analysis-step order. */
    public final List<TaxonomyCategory> getCategories() {
        return new ArrayList<>(mCategories.values());
    }

    public final TaxonomyCategory getCategory(final String name) {
        return mCategories.get(name);
    }

    public final List<TaxonomyCategory> getSupplementaryCategories() {
        return new ArrayList<>(mSupplementaryCategories.values());
    }

    public final TaxonomyCategory getSupplementaryCategory(final String name) {
        return mSupplementaryCategories.get(name);
    }

    public final List<BehaviorChannel> getChannels() {
        return new ArrayList<>(mChannels.values());
    }

    public final BehaviorChannel getChannel(final String name) {
        return mChannels.get(name);
    }

    /** Author-facing display groups, in display order. VSM-derived, no NEUROGES authority. */
    public final List<BehaviorDisplayGroup> getDisplayGroups() {
        return mDisplayGroups;
    }

    public final BehaviorDisplayGroup getDisplayGroup(final String id) {
        for (final BehaviorDisplayGroup group : mDisplayGroups) {
            if (group.getId().equals(id)) {
                return group;
            }
        }
        return null;
    }

    /**
     * The display group a command belongs to, or {@code null} if none claims it (control commands,
     * and anything untagged). Resolution: the first group listing the command's NEUROGES Function
     * wins; failing that, the first group listing its channel. Function takes precedence so that an
     * emotion preset on the {@code unknown} channel still lands in the Emotion group.
     */
    public final BehaviorDisplayGroup displayGroupOf(final BehaviorTag tag) {
        if (tag == null) {
            return null;
        }
        for (final BehaviorDisplayGroup group : mDisplayGroups) {
            if (group.matchesFunction(tag)) {
                return group;
            }
        }
        for (final BehaviorDisplayGroup group : mDisplayGroups) {
            if (group.matchesChannel(tag)) {
                return group;
            }
        }
        return null;
    }

    /** Convenience: {@link #displayGroupOf(BehaviorTag)} for a plugin/command pair. */
    public final BehaviorDisplayGroup displayGroupOf(final String plugin, final String command) {
        return displayGroupOf(tagFor(plugin, command));
    }

    /** Zero-based display order of a group id, or -1 if unknown. */
    public final int displayOrderOf(final String groupId) {
        for (int i = 0; i < mDisplayGroups.size(); i++) {
            if (mDisplayGroups.get(i).getId().equals(groupId)) {
                return i;
            }
        }
        return -1;
    }

    /** All command classifications, in file order. */
    public final List<BehaviorTag> getTags() {
        return new ArrayList<>(mTags.values());
    }

    /** Classification of one command, or {@code null} if the command is not tagged. */
    public final BehaviorTag tagFor(final String plugin, final String command) {
        return mTags.get(BehaviorTag.key(plugin, command));
    }

    /**
     * Whether a command is speech-accompanying and therefore a candidate for placement inside an
     * utterance. Untagged commands return false: an unclassified command must not be proposed.
     */
    public final boolean isCoSpeech(final String plugin, final String command) {
        final BehaviorTag tag = tagFor(plugin, command);
        return tag != null && tag.isCoSpeech();
    }

    /** Every tagged command carrying the given NEUROGES Function value. */
    public final List<BehaviorTag> tagsByFunction(final String function) {
        final List<BehaviorTag> out = new ArrayList<>();
        for (final BehaviorTag tag : mTags.values()) {
            if (function != null && function.equals(tag.getFunction())) {
                out.add(tag);
            }
        }
        return out;
    }

    /** Tagged co-speech commands, i.e. the candidate set a placement service may draw from. */
    public final List<BehaviorTag> coSpeechTags() {
        final List<BehaviorTag> out = new ArrayList<>();
        for (final BehaviorTag tag : mTags.values()) {
            if (tag.isCoSpeech()) {
                out.add(tag);
            }
        }
        return out;
    }

    /**
     * Function values with no tagged command at all — the repertoire gaps. Reported in
     * {@code doc/behavior-taxonomy-neuroges.md} §4.1 and the basis of the Xenia animation request.
     */
    public final List<String> uncoveredFunctionValues() {
        final TaxonomyCategory function = getCategory("function");
        if (function == null) {
            return Collections.emptyList();
        }
        final List<String> out = new ArrayList<>();
        for (final String value : function.getValues()) {
            if (tagsByFunction(value).isEmpty()) {
                out.add(value);
            }
        }
        return out;
    }

    /**
     * Adjacent values on a core category's polar axis — the back-off step for a placement model.
     * Empty for a non-polar or unknown category, or a value that is not on the axis.
     */
    public final List<String> neighboursOnAxis(final String category, final String value) {
        final TaxonomyCategory taxonomyCategory = getCategory(category);
        return taxonomyCategory == null
                ? Collections.<String>emptyList()
                : taxonomyCategory.neighboursOf(value);
    }

    /** Legal Type values under a Function value. Empty for Function values that have no Types
     *  (the two action values and emblem/social convention). */
    public final List<String> typesOf(final String function) {
        final TaxonomyCategory type = getCategory("type");
        if (type == null) {
            return Collections.emptyList();
        }
        final List<String> types = type.getTypesByFunction().get(function);
        return types == null ? Collections.emptyList() : types;
    }

    @Override
    public String toString() {
        return "BehaviorTaxonomy{" + mSystem + " " + mSystemVersion
                + ", " + mCategories.size() + " categories, " + mTags.size() + " tagged commands}";
    }
}
