package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.beans.HighlightInformation;

import javax.swing.text.BadLocationException;
import java.util.LinkedList;

public class ScriptHighlighter {

    private final LinkedList<Integer> matches;
    private final HighlightInformation documentInformation;
    private int currentHighlightedItem = -1;

    public ScriptHighlighter(HighlightInformation documentInformation) {
        this.documentInformation = documentInformation;
        matches = new LinkedList<>();
    }


    public void find() throws BadLocationException {
        reset();
        for (int index = 0;
             index + documentInformation.wordLength < documentInformation.documentLength;
             index++) {
            findMatchesInDocument(index);
        }

    }

    public void reset(){
        matches.clear();
        currentHighlightedItem = -1;

    }

    private void findMatchesInDocument(int index) throws BadLocationException {
        String match = documentInformation.document.getText(index, documentInformation.wordLength);
        if (documentInformation.wordToFind.equals(match)) {
            matches.add(index);
        }
    }

    public void next() throws BadLocationException {
        if (!hasNext()) return;
        updateNextPosition();
        highlightCurrentItem();

    }

    private void highlightCurrentItem() throws BadLocationException {
        int matchedWordStartIndex = matches.get(currentHighlightedItem);
        DocumentHighlighter documentHighlighter = new DocumentHighlighter(documentInformation);
        documentHighlighter.highlight(matchedWordStartIndex);
    }

    private void updateNextPosition() {
        if (hasNext()) {
            currentHighlightedItem++;
        }
    }

    private boolean hasNext() {
        return currentHighlightedItem < matches.size();
    }

    public void previous() throws BadLocationException {
        if (!hasPrevious()) return;
        updatePreviousPosition();
        highlightCurrentItem();
    }

    private void updatePreviousPosition() {
        if (hasPrevious()) {
            currentHighlightedItem--;
        }
    }

    private boolean hasPrevious() {
        return currentHighlightedItem > 0;
    }

    public void all() throws BadLocationException {
        for (Integer index: matches ) {
            next();
        }
    }

    public int totalOccurences() {
        return matches.size();
    }
}
