package de.dfki.vsm.model.behavior;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Guards the behavior taxonomy against drift.
 *
 * <p>Two kinds of check. <b>Structural</b> checks assert against the NEUROGES&reg; coding manual —
 * value sets, horizontal orders, the 24 Type values, the dependent Type/Function relation. They fail
 * if someone edits authoritative vocabulary without a {@code systemVersion} bump.
 * <b>Coverage</b> checks cross-reference every plugin's declared commands, so adding a behavior
 * command to a plugin without classifying it fails the build rather than silently leaving a hole in
 * the taxonomy.</p>
 *
 * <p>Plugin specs are read by relative path, matching {@code PluginCommandTest}.</p>
 */
class BehaviorTaxonomyTest {

    /** Plugins whose commands are behavior and must therefore be fully classified. */
    private static final String[] BEHAVIOR_PLUGINS = {"charamel-ws", "charamel-embed"};

    private static final Set<String> AFFILIATES = Set.of(
            "referent", "rheme", "accented-word", "clause", "whole-utterance", "none");

    private static BehaviorTaxonomy taxonomy() {
        return BehaviorTaxonomy.getDefault();
    }

    private static List<String> declaredCommands(String plugin) throws IOException {
        String text = Files.readString(
                Path.of("../plugins/" + plugin + "/src/main/resources/plugin-properties.json"));
        JSONObject root = new JSONObject(text);
        List<String> names = new ArrayList<>();
        if (root.optJSONArray("commands") != null) {
            root.optJSONArray("commands").forEach(entry ->
                    names.add(((JSONObject) entry).optString("name")));
        }
        return names;
    }

    // ---------------------------------------------------------------- loading

    @Test
    void loadsBundledResource() {
        BehaviorTaxonomy tax = taxonomy();
        assertEquals("NEUROGES", tax.getSystem());
        assertFalse(tax.getSystemVersion().isBlank(),
                "systemVersion must be set — it is what makes a NEUROGES revision a migration");
        assertEquals(List.of("upper-limbs", "lower-limbs", "head", "trunk"), tax.getBodyParts());
    }

    // ------------------------------------------------- authoritative vocabulary

    @Test
    void coreCategoriesMatchTheCodingManual() {
        BehaviorTaxonomy tax = taxonomy();
        assertEquals(7, tax.getCategories().size(), "NEUROGES has seven core categories");

        assertEquals(List.of("movement", "rest/pose"),
                tax.getCategory("activation").getValues());

        // Order is the axis: irregular -> repetitive -> phasic is increasing complexity. The 2009
        // values "continuous" and "stopped/holding" are NOT current and must not reappear.
        assertEquals(List.of("irregular", "repetitive", "phasic", "shift", "aborted"),
                tax.getCategory("structure").getValues());
        assertEquals(List.of("r/p rest", "r/p pose"),
                tax.getCategory("structure").getRestPoseValues());

        // Focus runs body-internal -> body-external.
        assertEquals(List.of("within body", "on body", "on attached object",
                        "on separate object", "on person", "in space"),
                tax.getCategory("focus").getValues());

        // Contact runs act on each other -> act as a unit -> act apart (decreasing bihemispheric
        // activation). Published prose elsewhere lists this reversed; the manual's order wins.
        assertEquals(List.of("act on each other", "act as a unit", "act apart"),
                tax.getCategory("contact").getValues());
        assertEquals(List.of("r/p crossed", "r/p closed", "r/p open"),
                tax.getCategory("contact").getRestPoseValues());

        // 2009's "complementary" and "independent" are gone.
        assertEquals(List.of("symmetrical", "right hand dominance", "left hand dominance", "asymmetrical"),
                tax.getCategory("formalRelation").getValues());

        assertEquals(11, tax.getCategory("function").getValues().size(),
                "the manual defines eleven Function values");
        assertEquals("emotion/attitude", tax.getCategory("function").getValues().get(0));
        assertEquals("emblem/social convention",
                tax.getCategory("function").getValues().get(10));
    }

    @Test
    void functionMainGroupsPartitionTheRepresentationalValues() {
        TaxonomyCategory function = taxonomy().getCategory("function");
        assertEquals("egocentric gestures", function.mainGroupOf("pantomime"));
        assertEquals("presentation gestures", function.mainGroupOf("spatial relation presentation"));
        assertEquals("actions", function.mainGroupOf("subject-oriented action"));
        // emphasis is the non-representational gesture value and sits in no main group.
        assertNull(function.mainGroupOf("emphasis"));

        for (Map.Entry<String, List<String>> group : function.getMainGroups().entrySet()) {
            for (String value : group.getValue()) {
                assertTrue(function.getValues().contains(value),
                        "main group " + group.getKey() + " references unknown Function value " + value);
            }
        }
    }

    @Test
    void typeIsDependentOnFunctionAndHas24Values() {
        BehaviorTaxonomy tax = taxonomy();
        TaxonomyCategory type = tax.getCategory("type");
        TaxonomyCategory function = tax.getCategory("function");

        Set<String> allTypes = new HashSet<>();
        for (Map.Entry<String, List<String>> entry : type.getTypesByFunction().entrySet()) {
            assertTrue(function.getValues().contains(entry.getKey()),
                    "typesByFunction keyed by unknown Function value: " + entry.getKey());
            allTypes.addAll(entry.getValue());
        }
        assertEquals(24, allTypes.size(), "the manual defines 24 Type values");
        assertEquals(function.getValues().size(), type.getTypesByFunction().size(),
                "every Function value needs an entry, even if its Type list is empty");

        // Actions and emblems have no Type values at all.
        assertTrue(tax.typesOf("object-oriented action").isEmpty());
        assertTrue(tax.typesOf("subject-oriented action").isEmpty());
        assertTrue(tax.typesOf("emblem/social convention").isEmpty());
        assertEquals(4, tax.typesOf("emphasis").size());
    }

    @Test
    void supplementaryCategoriesApplyToKnownFunctionValues() {
        BehaviorTaxonomy tax = taxonomy();
        assertEquals(7, tax.getSupplementaryCategories().size());
        List<String> functionValues = tax.getCategory("function").getValues();

        for (TaxonomyCategory supplementary : tax.getSupplementaryCategories()) {
            for (String applies : supplementary.getAppliesTo()) {
                assertTrue(functionValues.contains(applies),
                        supplementary.getName() + " appliesTo unknown Function value: " + applies);
            }
        }
        // Trigger/Motive is assessed only for subject-oriented actions — that restriction is what
        // grounds the cospeech:false decision for self-regulatory commands.
        assertEquals(List.of("subject-oriented action"),
                tax.getSupplementaryCategory("triggerMotive").getAppliesTo());
    }

    // ------------------------------------------------------------- polar axes

    @Test
    void polarAxesSupportNeighbourBackOff() {
        BehaviorTaxonomy tax = taxonomy();

        assertEquals(List.of("within body", "on attached object"),
                tax.neighboursOnAxis("focus", "on body"));
        // Axis ends have a single neighbour.
        assertEquals(List.of("on body"), tax.neighboursOnAxis("focus", "within body"));
        assertEquals(List.of("on person"), tax.neighboursOnAxis("focus", "in space"));

        // Rest/pose values are not on the movement axis, so they yield no neighbours.
        assertTrue(tax.neighboursOnAxis("structure", "r/p rest").isEmpty());
        // Non-polar categories never yield neighbours.
        assertTrue(tax.neighboursOnAxis("type", "emphasis-baton").isEmpty());
        assertTrue(tax.neighboursOnAxis("nonexistent", "whatever").isEmpty());

        assertEquals(0, tax.getCategory("structure").axisIndexOf("irregular"));
        assertEquals(-1, tax.getCategory("structure").axisIndexOf("r/p pose"));
    }

    // --------------------------------------------------------------- coverage

    @Test
    void everyDeclaredBehaviorCommandIsClassified() throws IOException {
        BehaviorTaxonomy tax = taxonomy();
        Set<String> missing = new TreeSet<>();
        for (String plugin : BEHAVIOR_PLUGINS) {
            for (String command : declaredCommands(plugin)) {
                if (tax.tagFor(plugin, command) == null) {
                    missing.add(plugin + "/" + command);
                }
            }
        }
        assertTrue(missing.isEmpty(),
                "commands declared in plugin-properties.json but absent from behavior-taxonomy.json: "
                        + missing);
    }

    @Test
    void noTagReferencesAnUndeclaredCommand() throws IOException {
        Set<String> declared = new HashSet<>();
        for (String plugin : BEHAVIOR_PLUGINS) {
            for (String command : declaredCommands(plugin)) {
                declared.add(plugin + "/" + command);
            }
        }
        Set<String> stale = new TreeSet<>();
        for (BehaviorTag tag : taxonomy().getTags()) {
            for (String plugin : BEHAVIOR_PLUGINS) {
                if (plugin.equals(tag.getPlugin()) && !declared.contains(tag.getKey())) {
                    stale.add(tag.getKey());
                }
            }
        }
        assertTrue(stale.isEmpty(),
                "behavior-taxonomy.json classifies commands no plugin declares any more: " + stale);
    }

    // ----------------------------------------------------------- tag validity

    @Test
    void everyTagUsesAuthoritativeValues() {
        BehaviorTaxonomy tax = taxonomy();
        List<String> problems = new ArrayList<>();

        for (BehaviorTag tag : tax.getTags()) {
            if (tax.getChannel(tag.getChannel()) == null) {
                problems.add(tag.getKey() + ": undeclared channel '" + tag.getChannel() + "'");
            }
            if (tag.getEvidence() == BehaviorTag.Evidence.UNKNOWN) {
                problems.add(tag.getKey() + ": evidence missing or unrecognised");
            }
            if (tag.getConfidence() < 0.0 || tag.getConfidence() > 1.0) {
                problems.add(tag.getKey() + ": confidence out of range: " + tag.getConfidence());
            }
            if (tag.getAffiliate() != null && !AFFILIATES.contains(tag.getAffiliate())) {
                problems.add(tag.getKey() + ": unknown affiliate '" + tag.getAffiliate() + "'");
            }
            for (Map.Entry<String, String> entry : tag.getNeuroges().entrySet()) {
                TaxonomyCategory category = tax.getCategory(entry.getKey());
                if (category == null) {
                    problems.add(tag.getKey() + ": unknown category '" + entry.getKey() + "'");
                } else if (!category.accepts(entry.getValue())) {
                    problems.add(tag.getKey() + ": '" + entry.getValue()
                            + "' is not a value of " + entry.getKey());
                }
            }
            for (Map.Entry<String, String> entry : tag.getSupplementary().entrySet()) {
                TaxonomyCategory category = tax.getSupplementaryCategory(entry.getKey());
                if (category == null) {
                    problems.add(tag.getKey() + ": unknown supplementary '" + entry.getKey() + "'");
                } else if (!category.accepts(entry.getValue())) {
                    problems.add(tag.getKey() + ": '" + entry.getValue()
                            + "' is not a value of supplementary " + entry.getKey());
                }
            }
        }
        assertTrue(problems.isEmpty(), "invalid tags:\n  " + String.join("\n  ", problems));
    }

    @Test
    void everyTaggedTypeIsLegalUnderItsFunction() {
        BehaviorTaxonomy tax = taxonomy();
        List<String> problems = new ArrayList<>();
        for (BehaviorTag tag : tax.getTags()) {
            String type = tag.getType();
            if (type == null) {
                continue;
            }
            String function = tag.getFunction();
            if (function == null) {
                problems.add(tag.getKey() + ": has a Type but no Function — Type is a dependent category");
            } else if (!tax.typesOf(function).contains(type)) {
                problems.add(tag.getKey() + ": Type '" + type + "' is not legal under Function '"
                        + function + "'");
            }
        }
        assertTrue(problems.isEmpty(), "Type/Function mismatches:\n  " + String.join("\n  ", problems));
    }

    @Test
    void restPoseCommandsCarryNoFunction() {
        // NEUROGES assesses Function and Type only for phasic and repetitive units. A posture must
        // therefore have no Function at all — absent, not null.
        BehaviorTaxonomy tax = taxonomy();
        List<String> problems = new ArrayList<>();
        for (BehaviorTag tag : tax.getTags()) {
            boolean restPose = "rest/pose".equals(tag.getValue("activation"))
                    || (tag.getValue("structure") != null && tag.getValue("structure").startsWith("r/p"));
            if (restPose && tag.getFunction() != null) {
                problems.add(tag.getKey() + ": rest/pose unit must not carry Function '"
                        + tag.getFunction() + "'");
            }
        }
        assertTrue(problems.isEmpty(), String.join("\n  ", problems));
    }

    // --------------------------------------------------- the three "no value" kinds

    @Test
    void distinguishesAbsentUndeterminedAndNoUnit() {
        BehaviorTaxonomy tax = taxonomy();

        // No NEUROGES unit at all: a backdrop change is not behavior.
        BehaviorTag background = tax.tagFor("charamel-ws", "background");
        assertNotNull(background);
        assertFalse(background.hasNeurogesUnit());
        assertFalse(background.isApplicable("function"));

        // Applicable but undetermined: name alone cannot settle it, video coding must.
        BehaviorTag explain = tax.tagFor("charamel-ws", "explain");
        assertNotNull(explain);
        assertTrue(explain.hasNeurogesUnit());
        assertTrue(explain.isApplicable("function"), "function key is present…");
        assertTrue(explain.isUndetermined("function"), "…but carries no value yet");
        assertNull(explain.getFunction());

        // Not applicable: a posture has no function key at all.
        BehaviorTag armscrossed = tax.tagFor("charamel-ws", "armscrossed");
        assertNotNull(armscrossed);
        assertTrue(armscrossed.hasNeurogesUnit());
        assertFalse(armscrossed.isApplicable("function"));
        assertFalse(armscrossed.isUndetermined("function"));
    }

    // ------------------------------------------------------- derived VSM views

    @Test
    void coSpeechGateExcludesPosturesStageAndControl() {
        BehaviorTaxonomy tax = taxonomy();

        assertTrue(tax.isCoSpeech("charamel-ws", "emphasis"));
        assertTrue(tax.isCoSpeech("charamel-ws", "pointopenpalm"));

        assertFalse(tax.isCoSpeech("charamel-ws", "armscrossed"), "a posture is not co-speech");
        assertFalse(tax.isCoSpeech("charamel-ws", "hairback"),
                "a self-regulatory action is state-driven, not utterance-placed");
        assertFalse(tax.isCoSpeech("charamel-ws", "background"), "stage effects are not behavior");
        assertFalse(tax.isCoSpeech("charamel-ws", "stop"));

        // An unclassified command must never be proposed for placement.
        assertFalse(tax.isCoSpeech("charamel-ws", "no-such-command"));
        assertFalse(tax.isCoSpeech("no-such-plugin", "emphasis"));
    }

    @Test
    void noTagIsReportableAsNeurogesGradeYet() {
        // Until the repertoire is video-coded, nothing here may be published as NEUROGES-grade.
        // When the first video-coded clips land this assertion flips — deliberately, not silently.
        List<String> graded = new ArrayList<>();
        for (BehaviorTag tag : taxonomy().getTags()) {
            if (tag.isNeurogesGrade()) {
                graded.add(tag.getKey());
            }
        }
        assertTrue(graded.isEmpty(),
                "tags now claim video-coded evidence — update doc/behavior-taxonomy-neuroges.md "
                        + "§5 and this test together: " + graded);
    }

    // ------------------------------------------------------- display reduction

    @Test
    void displayGroupsReferenceOnlyKnownFunctionsAndChannels() {
        BehaviorTaxonomy tax = taxonomy();
        assertFalse(tax.getDisplayGroups().isEmpty());
        List<String> functionValues = tax.getCategory("function").getValues();
        List<String> problems = new ArrayList<>();
        Set<String> ids = new HashSet<>();

        for (BehaviorDisplayGroup group : tax.getDisplayGroups()) {
            if (!ids.add(group.getId())) {
                problems.add("duplicate display group id: " + group.getId());
            }
            if (group.getLabel() == null || group.getLabel().isBlank()) {
                problems.add(group.getId() + ": missing label");
            }
            for (String function : group.getFunctions()) {
                if (!functionValues.contains(function)) {
                    problems.add(group.getId() + ": unknown Function value '" + function + "'");
                }
            }
            for (String channel : group.getChannels()) {
                if (tax.getChannel(channel) == null) {
                    problems.add(group.getId() + ": undeclared channel '" + channel + "'");
                }
            }
        }
        assertTrue(problems.isEmpty(), String.join("\n  ", problems));
    }

    @Test
    void everyFunctionValueIsReachableFromSomeDisplayGroup() {
        // The reduction may collapse Function values, but it must not drop any: a newly classified
        // command has to land in a column rather than vanish from the UI.
        BehaviorTaxonomy tax = taxonomy();
        Set<String> grouped = new HashSet<>();
        for (BehaviorDisplayGroup group : tax.getDisplayGroups()) {
            grouped.addAll(group.getFunctions());
        }
        Set<String> orphans = new TreeSet<>(tax.getCategory("function").getValues());
        orphans.removeAll(grouped);
        assertTrue(orphans.isEmpty(), "Function values no display group collapses: " + orphans);
    }

    @Test
    void resolvesDisplayGroupsFunctionFirstThenChannel() {
        BehaviorTaxonomy tax = taxonomy();

        // Function wins over channel: an emotion preset sits on the `unknown` channel (its body
        // involvement is undetermined) but still belongs in the Emotion column.
        assertEquals("unknown", tax.tagFor("charamel-ws", "happy").getChannel());
        assertEquals("emotion", tax.displayGroupOf("charamel-ws", "happy").getId());

        assertEquals("emphasis", tax.displayGroupOf("charamel-ws", "emphasis").getId());
        assertEquals("pointing", tax.displayGroupOf("charamel-ws", "pointdownleft").getId());
        assertEquals("convention", tax.displayGroupOf("charamel-ws", "nod").getId());
        assertEquals("action", tax.displayGroupOf("charamel-ws", "hairback").getId());

        // No Function, so the channel decides.
        assertEquals("background", tax.displayGroupOf("charamel-ws", "background").getId());
        assertEquals("posture", tax.displayGroupOf("charamel-ws", "armscrossed").getId());
        assertEquals("gaze", tax.displayGroupOf("charamel-ws", "lookleft").getId());
        assertEquals("face", tax.displayGroupOf("charamel-ws", "blink").getId());

        // Classification debt is shown, not hidden.
        assertEquals("unclassified", tax.displayGroupOf("charamel-ws", "explain").getId());

        // Control commands belong to no column at all.
        assertNull(tax.displayGroupOf("charamel-ws", "stop"));
        assertNull(tax.displayGroupOf("charamel-ws", "sequence"));
        assertNull(tax.displayGroupOf("no-such-plugin", "whatever"));
    }

    @Test
    void preservesTheSiaPanelColumnsThatExistToday() {
        // charamel-embed is the only previewCapable plugin, so these four commands are what the SIA
        // panel actually renders. Grouping by Function must not change the columns it shows today.
        BehaviorTaxonomy tax = taxonomy();
        assertEquals("background", tax.displayGroupOf("charamel-embed", "background").getId());
        assertEquals("emotion", tax.displayGroupOf("charamel-embed", "emotion").getId());
        assertNull(tax.displayGroupOf("charamel-embed", "stop"));
        assertNull(tax.displayGroupOf("charamel-embed", "clearemotion"));

        assertTrue(tax.getDisplayGroup("background").isSiaVisible());
        assertTrue(tax.getDisplayGroup("emotion").isSiaVisible());
        // Background before Emotion, as before.
        assertTrue(tax.displayOrderOf("background") < tax.displayOrderOf("emotion"));
    }

    @Test
    void reportsTheRepertoireGapDrivingTheAnimationRequest() {
        // Six Function values have no tagged command. Five of them are requested as new animations
        // (~/Code/Repo/xenia-animation); `object-oriented action` is deliberately not, because it
        // means changing the external physical world and a seated upper-body character has no props
        // to act on. That exclusion is character-specific and therefore lives in the animation
        // request, not in the taxonomy — which reports the gap as it is.
        assertEquals(List.of(
                        "egocentric direction",
                        "pantomime",
                        "form presentation",
                        "spatial relation presentation",
                        "motion quality presentation",
                        "object-oriented action"),
                taxonomy().uncoveredFunctionValues());
    }
}
