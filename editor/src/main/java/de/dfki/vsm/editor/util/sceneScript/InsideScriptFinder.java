package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;

import java.awt.*;


public class InsideScriptFinder extends MatchFinder{


    public static final Color SCRIPT_HIGHLIGHT_COLOR = new Color(255, 246, 209);

    public InsideScriptFinder(HighlightInformation documentInformation) {
        super(documentInformation);
    }

    @Override
    protected boolean isMatch(String match, int index) {
        return matchWord(match);
    }

    @Override
    public Color getColor() {
        return SCRIPT_HIGHLIGHT_COLOR;
    }
}
