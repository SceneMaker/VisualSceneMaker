package de.dfki.vsm.model.scenescript;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneScriptStructureTest {

    @Test
    void parsesSectionsAndNotesBetweenScenes() {
        String text = "# Introduction\n"
                + "\n"
                + "scene en warmup\n"
                + "Bob: Hello there.\n"
                + "\n"
                + "## Task 1\n"
                + "Note: remember to smile\n"
                + "\n"
                + "scene en task\n"
                + "Bob: Let's begin.\n";

        SceneScript script = new SceneScript();
        boolean ok = script.parseTXT(text);
        assertTrue(ok, "expected the script to parse successfully");

        List<ScriptEntity> entities = script.getEntityList();
        assertEquals(5, entities.size(), "expected [section, scene, section, note, scene]: " + entities);

        assertTrue(entities.get(0) instanceof SceneSection);
        assertEquals("# Introduction", entities.get(0).getText());

        assertTrue(entities.get(1) instanceof SceneObject);

        // "## Task 1" and "Note: ..." sit consecutively (no blank line needed between them) between the two scenes.
        assertTrue(entities.get(2) instanceof SceneSection);
        assertEquals("## Task 1", entities.get(2).getText());
        assertTrue(entities.get(3) instanceof SceneNote);
        assertEquals("Note: remember to smile", entities.get(3).getText());

        assertTrue(entities.get(4) instanceof SceneObject);

        // Offsets must point back at the exact original text.
        for (ScriptEntity entity : entities) {
            if (entity instanceof SceneSection || entity instanceof SceneNote) {
                String slice = text.substring(entity.getLower(), entity.getUpper());
                assertEquals(entity.getText(), slice, "offsets must match the original raw text exactly");
            }
        }
    }

    @Test
    void doesNotTreatNoteImmediatelyAfterTurnAsNewConstruct() {
        // No blank line before "Note:" — per the reachability rule this must NOT be recognized
        // as a structural entity (matches the traced YY_TURN_HEAD -> YYINITIAL blank-line rule).
        String text = "scene en warmup\n"
                + "Bob: Hello there.\n"
                + "Note: this looks like a note but has no blank line before it.\n";

        ScriptStructureScanner.ScanResult scan = ScriptStructureScanner.scan(text);
        assertTrue(scan.structuralEntities.isEmpty(), "a Note line right after a turn with no blank line separator should not be recognized");
        assertEquals(text, scan.sanitizedText, "nothing should have been blanked out");
    }

    @Test
    void diagnosticsStayCleanWithSectionsAndNotes() {
        String text = "# Introduction\n"
                + "\n"
                + "scene en warmup\n"
                + "Bob: Hello there.\n"
                + "\n"
                + "Note: remember to smile\n"
                + "\n"
                + "scene en task\n"
                + "Bob: Let's begin.\n";

        ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(text);
        assertTrue(result.isParseOk(), "sections/notes must not trigger false diagnostics: " + result.getDiagnostics());
    }

    @Test
    void xmlRoundTripEscapesSpecialCharacters() throws Exception {
        SceneNote note = new SceneNote(0, 10, "Note: \"quoted\" & special");
        java.io.ByteArrayOutputStream buffer = new java.io.ByteArrayOutputStream();
        de.dfki.vsm.util.ios.IOSIndentWriter writer = new de.dfki.vsm.util.ios.IOSIndentWriter(buffer);
        note.writeXML(writer);
        writer.flush();
        String xml = buffer.toString();
        assertTrue(xml.contains("&quot;quoted&quot;"), "quotes must be escaped: " + xml);
        assertTrue(xml.contains("&amp;"), "ampersand must be escaped: " + xml);
    }
}
