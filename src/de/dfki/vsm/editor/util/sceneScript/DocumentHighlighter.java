package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.beans.HighlightInformation;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.Highlighter;
import java.awt.*;

public class DocumentHighlighter {

    private static final Color SCENE_HIGHLIGHT_COLOR = new Color(56, 216, 120);
    private final HighlightInformation documentInformation;
    private final Color color;


    public DocumentHighlighter(HighlightInformation documentInformation) {
        this.documentInformation = documentInformation;
        this.color = SCENE_HIGHLIGHT_COLOR;
    }

    public DocumentHighlighter(HighlightInformation documentInformation, Color color) {
        this.documentInformation = documentInformation;
        this.color = color;
    }


    public void highlight(int startMatchIndex) throws BadLocationException {
        scrollToScene(startMatchIndex);
        highlightMatch(startMatchIndex);
    }

    private void highlightMatch(int startMatchIndex) throws BadLocationException {
        DefaultHighlighter.DefaultHighlightPainter highlighterPainter
                = new DefaultHighlighter.DefaultHighlightPainter(color);
        int endMatchIndex = startMatchIndex + documentInformation.wordLength;
        documentInformation.highlighter.addHighlight(startMatchIndex, endMatchIndex, highlighterPainter);
    }

    private void scrollToScene( int foundPosition) throws BadLocationException {
        Rectangle rView = documentInformation.editorPane.modelToView(foundPosition-1);
        rView.setSize((int)rView.getWidth(),(int)rView.getHeight() + 100);
        documentInformation.editorPane.scrollRectToVisible(rView);
    }
}
