package de.dfki.vsm.editor.project.auxiliary.script;

import de.dfki.vsm.model.scenescript.ScriptLexxer;
import de.dfki.vsm.util.syn.SyntaxEditorKit;

/**
 * @author Gregor Mehlmann
 */
public final class SceneScriptEditorKit extends SyntaxEditorKit {

    public SceneScriptEditorKit() {
        super(new ScriptLexxer(true, true, true));
    }
}
