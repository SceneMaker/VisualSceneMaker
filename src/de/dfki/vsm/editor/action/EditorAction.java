package de.dfki.vsm.editor.action;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * @author Not me
 */
public abstract class EditorAction {
    protected abstract void run();

    public ActionListener getActionListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                run();
            }
        };
    }
}
