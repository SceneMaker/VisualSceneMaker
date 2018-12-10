package de.dfki.vsm.editor.util.sceneScript.document;

import de.dfki.vsm.editor.util.sceneScript.MatchFinder;
import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import java.awt.*;

public class DocumentHighlighter {

    private static final Color SCENE_HIGHLIGHT_COLOR = new Color(56, 216, 120);
    private final HighlightInformation documentInformation;
    private final Color color;
    private final MatchFinder finder;


    public DocumentHighlighter(HighlightInformation documentInformation, MatchFinder finder) {
        this.documentInformation = documentInformation;
        this.color = SCENE_HIGHLIGHT_COLOR;
        this.finder = finder;
    }

    public DocumentHighlighter(HighlightInformation documentInformation, Color color, MatchFinder finder) {
        this.documentInformation = documentInformation;
        this.color = color;
        this.finder = finder;
    }

    public void highlight() throws BadLocationException {
        all();
    }

    public void next() throws BadLocationException {
        try {
            nextWithtRemoval();
        }catch (IndexOutOfBoundsException exception){
            System.out.println("No next");
        }

    }

    private void nextWithtRemoval() throws BadLocationException {
        try{
            int itemToHighlight = finder.next();
            finder.removeOldHighlights();
            highlightItem(itemToHighlight);
        }catch (IndexOutOfBoundsException exception){
            System.out.println("No next");
        }

    }

    private void nextWithoutRemoval() throws BadLocationException {
        try{
            int itemToHighlight = finder.next();
            highlightItem(itemToHighlight);
        }catch (IndexOutOfBoundsException exception){
            System.out.println("No next");
        }
    }


    public void previous() throws BadLocationException {
        try {
            int itemToHighlight = finder.previous();
            finder.removeOldHighlights();
            highlightItem(itemToHighlight);
        }catch (IndexOutOfBoundsException exception){
            System.out.println("No previous");
        }

    }

    public void all() throws BadLocationException {
        finder.find();
        while (finder.hasNext()){
            nextWithoutRemoval();
        }
        finder.resetOnlyCounter();
    }



    public int totalMatches(){
        return finder.getTotalMatches();
    }

    private void highlightItem(int startMatchIndex) throws BadLocationException {
        scrollToScene(startMatchIndex);
        highlightMatch(startMatchIndex);
    }

    private void highlightMatch(int startMatchIndex) throws BadLocationException {
        DefaultHighlighter.DefaultHighlightPainter highlighterPainter
                = new DefaultHighlighter.DefaultHighlightPainter(finder.getColor());
        int endMatchIndex = startMatchIndex + documentInformation.wordLength;
        documentInformation.highlighter.addHighlight(startMatchIndex, endMatchIndex, highlighterPainter);
    }

    private void scrollToScene( int foundPosition) throws BadLocationException {
        Rectangle rView = documentInformation.editorPane.modelToView(foundPosition-1);
        rView.setSize((int)rView.getWidth(),(int)rView.getHeight() + 100);
        documentInformation.editorPane.scrollRectToVisible(rView);
    }
}
