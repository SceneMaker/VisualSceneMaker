package de.dfki.vsm.editor.event;

import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Gregor Mehlmann
 */
public class SceneSelectedEvent extends EventObject {

    private SceneGroup mGroup;
    private String mLanguage;

    public SceneSelectedEvent(Object source, SceneGroup group) {
        super(source);
        mGroup = group;
    }

    public SceneGroup getGroup() {
        return mGroup;
    }

    public String getEventDescription() {
        return "SceneEvent(" + mGroup + ")";
    }
    
    public void setLanguage(String language) {
        mLanguage = language;
    }

    public String getLanguage() {
        return mLanguage.substring(mLanguage.indexOf('(') + 1, mLanguage.indexOf(')'));
    }
}
