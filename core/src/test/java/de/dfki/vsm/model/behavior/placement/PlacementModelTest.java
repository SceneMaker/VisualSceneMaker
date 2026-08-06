package de.dfki.vsm.model.behavior.placement;

import de.dfki.vsm.model.behavior.placement.PlacementContext.TurnPosition;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Behaviour of the placement model.
 *
 * <p>These pin the properties that make the model trustworthy on a tiny corpus: it says something
 * useful at n=0, it never spends confidence on a slot the sentence does not offer, one observation
 * cannot dominate, and a suggestion always reports what it rests on.
 */
class PlacementModelTest {

    private static final List<String> FULL_INVENTORY = List.of(
            "utterance-initial", "before-subject", "after-subject",
            "before-verb", "after-verb", "before-object", "after-object",
            "before-predicate", "after-predicate", "before-final-punct", "utterance-final");

    private static PlacementContext context(final String function, final String affiliate) {
        return new PlacementContext(function, affiliate, "main", TurnPosition.ONLY, null);
    }

    @Test
    void suggestsFromThePriorWhenNothingHasBeenObserved() {
        final PlacementModel model = PlacementModel.empty();
        final List<PlacementSuggestion> out = model.suggest(context(null, "referent"), FULL_INVENTORY, 3);

        assertFalse(out.isEmpty(), "a new project must still get a suggestion");
        assertEquals("before-object", out.get(0).getSlot(),
                "a referent-affiliated behavior belongs on the referent phrase");
        assertEquals(PlacementSuggestion.Basis.PRIOR, out.get(0).getBasis());
        assertTrue(out.get(0).isPriorOnly(),
                "an author must be able to tell a prior from evidence");
        assertEquals(0, out.get(0).getSupport());
    }

    @Test
    void priorDiffersByAffiliate() {
        final PlacementModel model = PlacementModel.empty();
        final String referentTop = model.suggest(context(null, "referent"), FULL_INVENTORY, 1).get(0).getSlot();
        final String clauseTop = model.suggest(context(null, "clause"), FULL_INVENTORY, 1).get(0).getSlot();
        final String utteranceTop =
                model.suggest(context(null, "whole-utterance"), FULL_INVENTORY, 1).get(0).getSlot();

        assertEquals("before-object", referentTop);
        assertEquals("before-final-punct", clauseTop,
                "clause-affiliated behavior goes to a clause edge; clause-initial is not offered here");
        assertEquals("utterance-initial", utteranceTop);
    }

    @Test
    void observationsOutrankThePrior() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext ctx = context("emblem/social convention", "rheme");
        for (int i = 0; i < 10; i += 1) {
            model.observe(ctx, "after-subject");
        }

        final List<PlacementSuggestion> out = model.suggest(ctx, FULL_INVENTORY, 3);
        assertEquals("after-subject", out.get(0).getSlot(),
                "ten observations must beat the prior, which would have said before-predicate");
        assertEquals(PlacementSuggestion.Basis.EXACT_CONTEXT, out.get(0).getBasis());
        assertFalse(out.get(0).isPriorOnly());
        assertTrue(out.get(0).getScore() > 0.5, "should be confident after ten consistent examples");
    }

    @Test
    void oneObservationInformsButDoesNotDominate() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext ctx = context("emotion/attitude", "rheme");
        model.observe(ctx, "utterance-final");

        final List<PlacementSuggestion> out = model.suggest(ctx, FULL_INVENTORY, 12);
        final double observed = scoreOf(out, "utterance-final");
        assertTrue(observed > 0.0, "the single observation must count for something");
        assertTrue(observed < 0.5,
                "one example must not carry a majority of the mass; got " + observed);
    }

    @Test
    void neverSpendsConfidenceOnASlotTheSentenceDoesNotOffer() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext ctx = context("emblem/social convention", "rheme");
        for (int i = 0; i < 10; i += 1) {
            model.observe(ctx, "after-subject");
        }

        // A subjectless sentence — "Super gemacht!" — offers no subject slots at all.
        final List<String> offered = List.of("utterance-initial", "before-verb", "utterance-final");
        final List<PlacementSuggestion> out = model.suggest(ctx, offered, 5);

        for (final PlacementSuggestion suggestion : out) {
            assertTrue(offered.contains(suggestion.getSlot()),
                    "suggested a slot that does not exist in this sentence: " + suggestion.getSlot());
        }
        final double total = out.stream().mapToDouble(PlacementSuggestion::getScore).sum();
        assertEquals(1.0, total, 1.0e-6,
                "mass must be renormalised over the offered slots, not lost to the absent one");
    }

    @Test
    void backsOffAlongTheNeurogesAxisToANeighbouringFunction() {
        final PlacementModel model = PlacementModel.empty();
        // Observed only under "emphasis" ...
        final PlacementContext observed = context("emphasis", "rheme");
        for (int i = 0; i < 8; i += 1) {
            model.observe(observed, "after-verb");
        }

        // ... and queried under "emotion/attitude", which sits next to it on the polar axis.
        final PlacementContext queried = context("emotion/attitude", "referent");
        final List<PlacementSuggestion> out = model.suggest(queried, FULL_INVENTORY, 5);

        assertEquals("after-verb", out.get(0).getSlot(),
                "an unobserved function should borrow from its axis neighbour before falling to the prior");
        assertEquals(PlacementSuggestion.Basis.AXIS_NEIGHBOUR, out.get(0).getBasis());
    }

    @Test
    void reportsUniformOnlyWhenNothingElseApplies() {
        final PlacementModel model = PlacementModel.empty();
        // An affiliate the prior does not cover, and no observations: nothing can rank these.
        final List<PlacementSuggestion> out =
                model.suggest(context(null, "not-an-affiliate"), List.of("before-verb"), 5);

        assertEquals(1, out.size());
        assertTrue(out.get(0).isPriorOnly());
    }

    @Test
    void offeringNoSlotsYieldsNoSuggestion() {
        final PlacementModel model = PlacementModel.empty();
        assertTrue(model.suggest(context("emotion/attitude", "rheme"), List.of(), 3).isEmpty(),
                "with no position to choose, staying silent beats inventing one");
    }

    @Test
    void ignoresPlacementsThatMatchNoKnownSlot() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext ctx = context("emotion/attitude", "rheme");
        model.observe(ctx, null);
        model.observe(ctx, "somewhere-in-the-middle");

        assertEquals(0, model.getObservationCount(),
                "a mid-phrase placement must not become evidence for an arbitrary slot");
        assertTrue(model.isEmpty());
    }

    @Test
    void countsSnappedPlacementsSeparatelySoTheApproximationStaysVisible() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext ctx = context("emotion/attitude", "rheme");
        model.observe(ctx, "after-subject");
        model.observeSnapped(ctx, "after-subject");

        assertEquals(2, model.getObservationCount());
        assertEquals(1, model.getSnappedCount(),
                "23% of real placements are mid-phrase; the model must not hide that it approximated");
    }

    @Test
    void survivesARoundTripThroughJson() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext ctx = context("emblem/social convention", "rheme");
        for (int i = 0; i < 4; i += 1) {
            model.observe(ctx, "after-subject");
        }
        model.observeSnapped(ctx, "before-verb");

        final JSONObject json = model.toJson();
        assertEquals(PlacementModel.MODEL_VERSION, json.getInt("version"));

        final PlacementModel restored = PlacementModel.fromJson(json);
        assertEquals(model.getObservationCount(), restored.getObservationCount());
        assertEquals(model.getSnappedCount(), restored.getSnappedCount());
        assertEquals(
                model.suggest(ctx, FULL_INVENTORY, 3).get(0).getSlot(),
                restored.suggest(ctx, FULL_INVENTORY, 3).get(0).getSlot());
    }

    @Test
    void turnPositionSeparatesContexts() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext first =
                new PlacementContext("emotion/attitude", "rheme", "main", TurnPosition.FIRST, null);
        final PlacementContext last =
                new PlacementContext("emotion/attitude", "rheme", "main", TurnPosition.LAST, null);
        for (int i = 0; i < 6; i += 1) {
            model.observe(first, "utterance-initial");
            model.observe(last, "utterance-final");
        }

        assertEquals("utterance-initial", model.suggest(first, FULL_INVENTORY, 1).get(0).getSlot());
        assertEquals("utterance-final", model.suggest(last, FULL_INVENTORY, 1).get(0).getSlot());
    }

    @Test
    void turnPositionOfHandlesEdges() {
        assertEquals(TurnPosition.ONLY, TurnPosition.of(0, 1));
        assertEquals(TurnPosition.FIRST, TurnPosition.of(0, 3));
        assertEquals(TurnPosition.MIDDLE, TurnPosition.of(1, 3));
        assertEquals(TurnPosition.LAST, TurnPosition.of(2, 3));
        assertEquals(TurnPosition.UNKNOWN, TurnPosition.of(0, 0));
    }

    @Test
    void scoresSumToOneOverTheOfferedSlots() {
        final PlacementModel model = PlacementModel.empty();
        model.observe(context("emotion/attitude", "rheme"), "after-subject");

        final List<PlacementSuggestion> out =
                model.suggest(context("emotion/attitude", "rheme"), FULL_INVENTORY, 0);
        final double total = out.stream().mapToDouble(PlacementSuggestion::getScore).sum();
        assertEquals(1.0, total, 1.0e-6, "the ranking is a distribution, so it must normalise");
    }

    @Test
    void differentFunctionsCanLearnDifferentSlots() {
        final PlacementModel model = PlacementModel.empty();
        final PlacementContext emblem = context("emblem/social convention", "rheme");
        final PlacementContext emotion = context("subject-oriented action", "referent");
        for (int i = 0; i < 8; i += 1) {
            model.observe(emblem, "after-subject");
            model.observe(emotion, "before-object");
        }

        assertNotEquals(
                model.suggest(emblem, FULL_INVENTORY, 1).get(0).getSlot(),
                model.suggest(emotion, FULL_INVENTORY, 1).get(0).getSlot(),
                "conditioning on function is the point; the two must not collapse together");
    }

    private static double scoreOf(final List<PlacementSuggestion> out, final String slot) {
        return out.stream()
                .filter(s -> slot.equals(s.getSlot()))
                .mapToDouble(PlacementSuggestion::getScore)
                .findFirst()
                .orElse(0.0);
    }
}
