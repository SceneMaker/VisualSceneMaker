package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.scenescript.SceneGroup;

/**
 * @author Gregor Mehlmann
 */
public class SceneSelectedEvent extends EventObject {
    private SceneGroup mGroup;
    private String     mLanguage;

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
