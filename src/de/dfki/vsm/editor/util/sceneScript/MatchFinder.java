package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;
import de.dfki.vsm.editor.util.sceneScript.interfaces.BackwardsIterator;

import javax.swing.text.BadLocationException;
import java.awt.*;
import java.util.LinkedList;

abstract public class MatchFinder implements BackwardsIterator {
    protected final HighlightInformation documentInformation;
    private LinkedList<Integer> matches;
    private int currentHighlightedItem = 0;
    private boolean started = false;

    public MatchFinder(HighlightInformation documentInformation) {
        matches = new LinkedList<>();
        this.documentInformation = documentInformation;
    }


    @Override
    public boolean hasNext() {
        return currentHighlightedItem < matches.size();
    }

    @Override
    public Integer next() {
        if (!hasNext()) throw new IndexOutOfBoundsException();
        startFinderIfNeeded();
        int item = getCurrentItem();
        updateNextPosition();
        return item;
    }

    public void startFinderIfNeeded() {
        if (!started) {
            init();
        }
    }

    private void init() {
        try {
            find();
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
    }

    private Integer getCurrentItem() {
        return matches.get(currentHighlightedItem);
    }

    private void updateNextPosition() {
        if (hasNext()) {
            currentHighlightedItem++;
        }
    }

    public void reset() {
        currentHighlightedItem = 0;
        matches.clear();
    }

    @Override
    public boolean hasPrevious() {
        return currentHighlightedItem > 0;
    }

    @Override
    public Integer previous() {
        if (!hasPrevious()) throw new IndexOutOfBoundsException();
        startFinderIfNeeded();
        int item = getCurrentItem();
        updatePreviousPosition();
        return item;
    }

    private void updatePreviousPosition() {
        if (hasPrevious()) {
            currentHighlightedItem--;
        }
    }


    public void find() throws BadLocationException {
        reset();
        started = true;
        for (int index = 0;
             index + documentInformation.wordLength < documentInformation.documentLength;
             index++) {
            findMatchesInDocument(index);
        }
    }


    private void findMatchesInDocument(int index) throws BadLocationException {
        String match = documentInformation.document.getText(index, documentInformation.wordLength);
        if (isMatch(match, index)) {
            matches.add(index);
        }
    }

    protected boolean matchWord(String match) {
        return documentInformation.wordToFind.equals(match);
    }

    protected abstract boolean isMatch(String match, int index);

    public abstract Color getColor();


    public int getTotalMatches() {
        return matches.size();
    }
}
