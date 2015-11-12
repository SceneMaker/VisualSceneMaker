package de.dfki.vsm.editor.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.evt.EventObject;

/**
 * @author M. Fallas
 */
public class ExceptionThrownEvent extends EventObject {

    //THIS EVENT IS THROWN WHEN A InterpetException IS LAUNCHED
    //IN ORDER TO STOP PROPERLY THE EXECUTION OF THE CURRENT SCENEFLOW
    private String description;
    public ExceptionThrownEvent(Object source, String msg) {
        super(source);
        description = msg;
    }


    public String getEventDescription() {
        return description;
    }
}
