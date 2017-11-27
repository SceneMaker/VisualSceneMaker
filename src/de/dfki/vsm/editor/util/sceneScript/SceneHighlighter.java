package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.beans.HighlightInformation;

import javax.swing.text.BadLocationException;
import java.awt.*;

public class SceneHighlighter extends MatchFinder{

    public static final Color SCENE_HIGHLIGHT_COLOR = new Color(56, 216, 120);

    public SceneHighlighter(HighlightInformation documentInformation) {
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
