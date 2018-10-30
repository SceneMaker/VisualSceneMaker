package de.dfki.vsm.editor.project.auxiliary.scenescript;

import de.dfki.vsm.model.scenescript.ScriptLexxer;
import de.dfki.vsm.util.syn.SyntaxEditorKit;

/**
 * @author Gregor Mehlmann
 */
public final class ScriptEditorKit extends SyntaxEditorKit {

    public ScriptEditorKit() {
        super(new ScriptLexxer(true, true, true));
    }
}
