package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;

import javax.swing.text.BadLocationException;
import java.awt.*;

public class SceneFinder extends MatchFinder{

    public static final Color SCENE_HIGHLIGHT_COLOR = new Color(56, 216, 120);

    public SceneFinder(HighlightInformation documentInformation) {
        super(documentInformation);
    }


    @Override
    protected boolean isMatch(String match, int wordStartIndex) {
        try {
            return isScene(wordStartIndex) && matchWord(match);
        } catch (BadLocationException e) {
           return false;
        }

    }


    private boolean isScene(int index) throws BadLocationException {
        return documentInformation.document.getText(index + documentInformation.wordLength, 1).equals("\n");
    }
}
