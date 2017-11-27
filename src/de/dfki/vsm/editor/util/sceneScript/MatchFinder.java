package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;
import de.dfki.vsm.editor.util.sceneScript.interfaces.BackwardsIterator;

import javax.swing.text.BadLocationException;
import java.util.LinkedList;

abstract public class MatchFinder implements BackwardsIterator{
    protected final HighlightInformation documentInformation;
    private LinkedList<Integer> matches;
    private int currentHighlightedItem = -1;

    public MatchFinder(HighlightInformation documentInformation){
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
        updateNextPosition();
        return getCurrentItem();
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
        currentHighlightedItem = -1;
        matches.clear();
    }

    @Override
    public boolean hasPrevious() {
        return currentHighlightedItem > 0;
    }

    @Override
    public Integer previous() {
        if (!hasPrevious()) throw new IndexOutOfBoundsException();
        updatePreviousPosition();
        return getCurrentItem();
    }

    private void updatePreviousPosition() {
        if (hasPrevious()) {
            currentHighlightedItem--;
        }
    }


    public void find() throws BadLocationException {
        reset();
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

    protected boolean matchWord(String match){
        return documentInformation.wordToFind.equals(match);
    }

    protected abstract boolean isMatch(String match, int index) ;



}
