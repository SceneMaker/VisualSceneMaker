package de.dfki.vsm.model.scenescript;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Recognizes the top-level-only "Section" (markdown ATX header, e.g. "# Introduction")
 * and "Note" (e.g. "Note: remember to smile") constructs without touching the
 * generated JFlex/CUP scene-script grammar.
 *
 * A line qualifies only where the real grammar could reach its top-level state: the
 * first line of the document, right after a blank line, or right after another
 * recognized Section/Note line. Each recognized line is extracted as its own
 * {@link SceneSection}/{@link SceneNote} entity and blanked out (replaced with
 * same-length whitespace) in a sanitized copy of the text, so the existing
 * lexer/parser can run over the sanitized text completely unchanged while still
 * producing offsets valid against the original text.
 */
final class ScriptStructureScanner {

    private static final Pattern SECTION_LINE = Pattern.compile("^#{1,3}[ \\t]+\\S.*$");
    private static final Pattern NOTE_LINE = Pattern.compile("^(Note:|NOTE:).*$");

    static final class ScanResult {
        final List<ScriptEntity> structuralEntities;
        final String sanitizedText;

        private ScanResult(final List<ScriptEntity> structuralEntities, final String sanitizedText) {
            this.structuralEntities = structuralEntities;
            this.sanitizedText = sanitizedText;
        }
    }

    private ScriptStructureScanner() {}

    static ScanResult scan(final String text) {
        final List<ScriptEntity> entities = new ArrayList<>();
        if (text == null || text.isEmpty()) {
            return new ScanResult(entities, text == null ? "" : text);
        }

        final char[] sanitized = text.toCharArray();
        boolean atBoundary = true;
        int lineStart = 0;

        while (lineStart <= text.length()) {
            int lineEnd = text.indexOf('\n', lineStart);
            if (lineEnd < 0) {
                lineEnd = text.length();
            }
            final String line = text.substring(lineStart, lineEnd);

            if (line.trim().isEmpty()) {
                atBoundary = true;
            } else if (atBoundary && SECTION_LINE.matcher(line).matches()) {
                entities.add(new SceneSection(lineStart, lineEnd, line));
                blank(sanitized, lineStart, lineEnd);
                atBoundary = true;
            } else if (atBoundary && NOTE_LINE.matcher(line).matches()) {
                entities.add(new SceneNote(lineStart, lineEnd, line));
                blank(sanitized, lineStart, lineEnd);
                atBoundary = true;
            } else {
                atBoundary = false;
            }

            if (lineEnd >= text.length()) {
                break;
            }
            lineStart = lineEnd + 1;
        }

        return new ScanResult(entities, new String(sanitized));
    }

    private static void blank(final char[] chars, final int from, final int to) {
        for (int i = from; i < to; i++) {
            chars[i] = ' ';
        }
    }
}
