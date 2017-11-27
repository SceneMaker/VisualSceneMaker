package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.document.DocumentHighlighter;
import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;

import javax.swing.text.BadLocationException;
import java.util.LinkedList;

public class ScriptHighlighter {

   /* private final LinkedList<Integer> matches;
    private final HighlightInformation documentInformation;
    private final MatchFinder finder;

    public ScriptHighlighter(HighlightInformation documentInformation, MatchFinder finder) {
        matches = new LinkedList<>();
        this.documentInformation = documentInformation;
        this.finder = finder;
    }


    public void next() throws BadLocationException {
        int wordIndex = finder.next();
        highlightCurrentItem(wordIndex);

    }

    private void highlightCurrentItem(int wordIndex) throws BadLocationException {
        int matchedWordStartIndex = matches.get(wordIndex);
        DocumentHighlighter documentHighlighter = new DocumentHighlighter(documentInformation);
        documentHighlighter.highlight(matchedWordStartIndex);
    }


    public void all() throws BadLocationException {
        for (Integer index: matches ) {
            int matchedWordStartIndex = finder.next();
            highlightCurrentItem(matchedWordStartIndex);
        }
    }

    public int totalOccurences() {
        return matches.size();
    }*/
}
