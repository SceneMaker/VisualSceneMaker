package de.dfki.vsm.web;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Validation of the LLM second opinion (plan 3.4).
 *
 * <p>The feature degrades safely in every direction except one: a slot that is accepted but wrong
 * would put a real suggestion in front of an author with no way to tell it apart from evidence.
 * These tests pin the closed output space — anything not literally one of the offered slot names is
 * refused, never repaired.
 */
class PlacementSecondOpinionTest {

    private static final List<String> OFFERED =
            List.of("utterance-initial", "after-subject", "before-predicate");

    @Test
    void acceptsASlotFromTheOfferedInventory() {
        JSONObject out = WebUiServer.parseSecondOpinion(
                "{\"slot\":\"after-subject\",\"reason\":\"greeting emblem follows the subject\"}", OFFERED);

        assertNotNull(out);
        assertEquals("after-subject", out.getString("slot"));
        assertEquals("llm", out.getString("source"),
                "provenance must survive, so a suggestion stays attributable");
        assertEquals("greeting emblem follows the subject", out.getString("reason"));
    }

    @Test
    void stripsMarkdownFencing() {
        JSONObject out = WebUiServer.parseSecondOpinion(
                "```json\n{\"slot\":\"before-predicate\"}\n```", OFFERED);

        assertNotNull(out);
        assertEquals("before-predicate", out.getString("slot"));
        assertFalse(out.has("reason"), "an absent reason must not become an empty one");
    }

    @Test
    void rejectsASlotThatIsNotOffered() {
        // A real slot name, but not one this sentence has — the sentence may have no object at all.
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"after-object\"}", OFFERED));
    }

    @Test
    void rejectsAnInventedSlot() {
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"just-after-the-verb-ish\"}", OFFERED));
    }

    @Test
    void rejectsProseWhereANameWasAskedFor() {
        assertNull(WebUiServer.parseSecondOpinion(
                "I would put it right after the subject.", OFFERED));
    }

    @Test
    void rejectsACharacterOffsetInsteadOfASlot() {
        // The one answer shaped like success but meaning something else entirely.
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"12\"}", OFFERED));
    }

    @Test
    void rejectsMalformedJson() {
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\": ", OFFERED));
    }

    @Test
    void rejectsEmptyAndNullReplies() {
        assertNull(WebUiServer.parseSecondOpinion("", OFFERED));
        assertNull(WebUiServer.parseSecondOpinion("   ", OFFERED));
        assertNull(WebUiServer.parseSecondOpinion(null, OFFERED));
    }

    @Test
    void rejectsEverythingWhenNoSlotsAreOffered() {
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"after-subject\"}", List.of()));
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"after-subject\"}", null));
    }

    @Test
    void doesNotAcceptASlotByPrefixOrCase() {
        // Matching must be exact: "after-subj" and "After-Subject" are not the label the corpus uses,
        // and accepting either would put an unmatchable slot into the model's vocabulary.
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"after-subj\"}", OFFERED));
        assertNull(WebUiServer.parseSecondOpinion("{\"slot\":\"After-Subject\"}", OFFERED));
    }
}
