package de.dfki.vsm.model.scenescript;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the spoken-text projection and, with it, the two verified defects of the former
 * browser-side pipeline: inline commands reaching the parser, and a regex sentence splitter cutting
 * a command in half at a parameter's decimal point.
 */
class UtteranceProjectionTest {

    private static SceneTurn firstTurn(String text) {
        SceneScript script = new SceneScript();
        assertTrue(script.parseTXT(text), "script must parse: " + text);
        assertEquals(1, script.getSceneList().size());
        return script.getSceneList().get(0).getTurnList().get(0);
    }

    private static List<UtteranceProjection> sentences(String text) {
        return UtteranceProjection.sentencesOf(firstTurn(text));
    }

    // ------------------------------------------------------- commands removed

    @Test
    void removesInlineCommandsFromTheTextTheParserSees() {
        String text = "scene de welcome\n"
                + "Xenia: [background color='#77bb41'] Hallo [emotion type='happy'] ich bin Xenia.\n";
        List<UtteranceProjection> out = sentences(text);
        assertEquals(1, out.size());

        UtteranceProjection p = out.get(0);
        assertEquals("Hallo ich bin Xenia.", p.getCleanText());
        assertEquals(4, p.getTokenCount());
        assertFalse(p.getCleanText().contains("["), "no bracket may survive into the parser input");
        assertFalse(p.getCleanText().contains("#77bb41"));
    }

    @Test
    void recordsEachCommandsGapIndex() {
        String text = "scene de welcome\n"
                + "Xenia: [background color='#77bb41'] Hallo [emotion type='happy'] ich bin Xenia.\n";
        UtteranceProjection p = sentences(text).get(0);

        assertEquals(2, p.getCommands().size());
        // Utterance-initial: no spoken token precedes it.
        assertEquals("background", p.getCommands().get(0).getName());
        assertEquals(0, p.getCommands().get(0).getTokenIndex());
        assertEquals(0, p.getCommands().get(0).getCleanOffset());
        // After "Hallo".
        assertEquals("emotion", p.getCommands().get(1).getName());
        assertEquals(1, p.getCommands().get(1).getTokenIndex());
        assertEquals("Hallo".length(), p.getCommands().get(1).getCleanOffset());
    }

    @Test
    void unqualifiedCommandHasEmptyActorNotNull() {
        UtteranceProjection p = sentences("scene de a\nXenia: [emotion type='sad'] Nun gut.\n").get(0);
        assertEquals("", p.getCommands().get(0).getActor());
    }

    @Test
    void commandSpanCoversTheActorQualifiedName() {
        // ActionObject records only the actor-qualified name in lower/upper, not the whole bracket:
        // "time: init" for [time: init id='one']. Pinned because it looks like an off-by-something
        // when you slice the script with it and expect just the command name.
        String text = "scene de a\nXenia: Hallo [time: init id='one'] jetzt.\n";
        UtteranceProjection p = sentences(text).get(0);

        UtteranceProjection.CommandPosition command = p.getCommands().get(0);
        assertEquals("init", command.getName());
        assertEquals("time", command.getActor());
        assertEquals("time: init",
                text.substring(command.getScriptFrom(), command.getScriptTo()));
    }

    @Test
    void adjacentCommandsShareOneGapIndex() {
        // Three commands between two words all sit at the same structural position. Collapsing them
        // to one gap index is correct: the placement label is a boundary, not a character offset.
        String text = "scene de a\n"
                + "Xenia: Hallo [pause duration='50'] [emotion type='happy'] [background color='#fff'] ich bin da.\n";
        UtteranceProjection p = sentences(text).get(0);

        assertEquals("Hallo ich bin da.", p.getCleanText());
        assertEquals(3, p.getCommands().size());
        for (UtteranceProjection.CommandPosition command : p.getCommands()) {
            assertEquals(1, command.getTokenIndex(), command.getName() + " should sit in the gap after 'Hallo'");
        }
    }

    // ------------------------------------------- the old splitter's decimal defect

    @Test
    void doesNotSplitInsideACommandParameter() {
        // The former regex splitter (/[^.!?]+[.!?]+|[^.!?]+$/) cut this into
        //   "Schön [emotion type='happy' intensity='0."  +  "8'] dass Du da bist."
        // Driving off the parsed model makes that impossible by construction.
        String text = "scene de two\n"
                + "Xenia: Schön [emotion type='happy' intensity='0.8'] dass Du da bist.\n";
        List<UtteranceProjection> out = sentences(text);

        assertEquals(1, out.size(), "a decimal inside a parameter must not create a second sentence");
        assertEquals("Schön dass Du da bist.", out.get(0).getCleanText());
        assertEquals(1, out.get(0).getCommands().size());
        assertEquals(1, out.get(0).getCommands().get(0).getTokenIndex());
    }

    // ---------------------------------------------------- sentence grouping

    @Test
    void mergesCommaSplitUtterancesIntoOneSentence() {
        // The script grammar ends an utterance at any punctuation, so this parses as two
        // SceneUttrs. Handing "Hallo $user," to a parser alone would be handing it a fragment and
        // would defeat the UD service's greeting guardrail.
        String text = "scene de ph\nXenia: Hallo $user, wie geht's Dir?\n";
        SceneTurn turn = firstTurn(text);
        assertEquals(2, turn.getUttrList().size(), "precondition: the comma splits the utterance");

        List<UtteranceProjection> out = UtteranceProjection.sentencesOf(turn);
        assertEquals(1, out.size(), "the comma must not end the sentence");
        assertEquals("Hallo $user, wie geht's Dir?", out.get(0).getCleanText());
        assertEquals(5, out.get(0).getTokenCount());
    }

    @Test
    void splitsOnSentenceFinalPunctuation() {
        List<UtteranceProjection> out = sentences("scene de m\nXenia: Hallo. Wie geht es Dir?\n");
        assertEquals(2, out.size());
        assertEquals("Hallo.", out.get(0).getCleanText());
        assertEquals("Wie geht es Dir?", out.get(1).getCleanText());
    }

    @Test
    void keepsPlaceholdersAndAbbreviationsVerbatim() {
        // SceneUttr.getCleanText() drops SceneParam and SceneAbbrev; the projection must not, or the
        // parser would see a different sentence. Placeholder normalisation is the UD service's job.
        UtteranceProjection p = sentences("scene de ph\nXenia: Hallo $user, wie geht's Dir?\n").get(0);
        assertTrue(p.getCleanText().contains("$user"), "placeholder must survive: " + p.getCleanText());
        assertTrue(p.getCleanText().contains("geht's"), "abbreviation must survive: " + p.getCleanText());
    }

    // --------------------------------------------------------- offset mapping

    @Test
    void mapsEveryTokenSpanBackToTheOriginalScriptText() {
        String text = "scene de welcome\n"
                + "Xenia: [background color='#77bb41'] Hallo [emotion type='happy'] ich bin Xenia.\n";
        UtteranceProjection p = sentences(text).get(0);

        for (UtteranceProjection.Token token : p.getTokens()) {
            int[] span = p.toScriptSpan(token.getCleanFrom(), token.getCleanTo());
            assertEquals(token.getScriptFrom(), span[0], "start of " + token);
            assertEquals(token.getScriptTo(), span[1], "end of " + token);
            if (!token.isPunctuation()) {
                assertEquals(token.getText(), text.substring(span[0], span[1]),
                        "a mapped span must slice the original script back to the token text");
            }
        }
    }

    @Test
    void mapsAMultiTokenPhraseSpan() {
        String text = "scene de welcome\n"
                + "Xenia: [background color='#77bb41'] Hallo [emotion type='happy'] ich bin Xenia.\n";
        UtteranceProjection p = sentences(text).get(0);

        // "ich bin Xenia" in clean text -> the corresponding stretch of the script, which in the
        // original is separated from "Hallo" by a whole command bracket. This is why the mapping
        // cannot be a constant offset.
        int from = p.getCleanText().indexOf("ich");
        int to = p.getCleanText().indexOf("Xenia") + "Xenia".length();
        int[] span = p.toScriptSpan(from, to);
        assertEquals("ich bin Xenia", text.substring(span[0], span[1]));
    }

    @Test
    void snapsGapOffsetsOutwardRatherThanIntoRemovedMaterial() {
        String text = "scene de welcome\n"
                + "Xenia: [background color='#77bb41'] Hallo [emotion type='happy'] ich bin Xenia.\n";
        UtteranceProjection p = sentences(text).get(0);

        // The single space between "Hallo" and "ich" in clean text stands for a removed command in
        // the script. An end offset there must snap back to the end of "Hallo", a start offset
        // forward to the beginning of "ich" — never into the bracket.
        int gap = p.getCleanText().indexOf(' ');
        assertEquals(text.indexOf("Hallo") + "Hallo".length(), p.toScriptOffset(gap, true));
        assertEquals(text.indexOf("ich"), p.toScriptOffset(gap + 1, false));
    }

    @Test
    void reportsNoCleanOffsetForScriptPositionsInsideACommand() {
        String text = "scene de welcome\n"
                + "Xenia: [background color='#77bb41'] Hallo [emotion type='happy'] ich bin Xenia.\n";
        UtteranceProjection p = sentences(text).get(0);

        int insideCommand = text.indexOf("#77bb41");
        assertEquals(-1, p.toCleanOffset(insideCommand),
                "a script offset inside a removed command has no clean-text counterpart");

        int insideWord = text.indexOf("Hallo") + 1;
        assertEquals(p.getCleanText().indexOf("Hallo") + 1, p.toCleanOffset(insideWord));
    }

    // ------------------------------------------------------------ degenerate input

    @Test
    void toleratesNullAndEmptyInput() {
        assertTrue(UtteranceProjection.of((SceneUttr) null).isEmpty());
        assertTrue(UtteranceProjection.sentencesOf(null).isEmpty());
        UtteranceProjection empty = UtteranceProjection.of((SceneUttr) null);
        assertEquals(0, empty.getTokenCount());
        assertEquals("", empty.getCleanText());
        // Mapping on an empty projection must not throw.
        assertEquals(2, empty.toScriptSpan(0, 5).length);
    }
}
