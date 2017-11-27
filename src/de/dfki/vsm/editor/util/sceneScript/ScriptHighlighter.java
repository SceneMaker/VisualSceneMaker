package de.dfki.vsm.editor.util.sceneScript;

import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;


public class ScriptHighlighter extends MatchFinder{


    public ScriptHighlighter(HighlightInformation documentInformation) {
        super(documentInformation);
    }

    @Override
    protected boolean isMatch(String match, int index) {
        return matchWord(match);
    }
}
