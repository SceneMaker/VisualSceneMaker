package de.dfki.vsm.runtime.exceptions;

import de.dfki.vsm.editor.event.ExceptionThrownEvent;
import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * @author Gregor Mehlmann
 */
public final class InterpretException extends RunTimeException {
    private final EventDispatcher  mEventCaster = EventDispatcher.getInstance();
    
    public InterpretException(final Object obj, final String msg) {
        super(obj, msg);
        //SENT THE EVENT THAT WILL LAUNCH THE ABORTION OF THE SCENEFLOW'S EXECUTION
        mEventCaster.convey(new ExceptionThrownEvent(this, msg));
    }
}
