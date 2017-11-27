package de.dfki.vsm.editor.util.sceneScript.document.beans;

import javax.swing.*;
import javax.swing.text.Document;
import javax.swing.text.Highlighter;
import java.util.LinkedList;

public class HighlightInformation {

    public final Document document;
    public final String wordToFind;
    public final Highlighter highlighter;
    public final LinkedList<Integer> matches;
    public final JEditorPane editorPane;
    public final int documentLength;
    public final int wordLength;

    public HighlightInformation(Document document, String wordToFind, Highlighter highlighter, JEditorPane editorPane){
        this.document = document;
        this.wordToFind = wordToFind;
        this.highlighter = highlighter;
        this.matches = new LinkedList<>();
        this.editorPane = editorPane;
        this.documentLength = document.getLength();
        this.wordLength = wordToFind.length();

    }
}
