package de.dfki.vsm.editor.action;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author Gregor Mehlmann
 */
public abstract class EditorAction {
    protected abstract void run();

    public ActionListener getActionListener() {
        return event -> run();
    }
}
