package de.dfki.vsm.xtension.stickmantts.util.tts.events;

//~--- non-JDK imports --------------------------------------------------------

//import de.dfki.embots.output.scenePlayer.scenes.Scene;
import de.dfki.vsm.util.evt.EventObject;

/**
 * @author Sergio Soto
 */

public class LineStart extends EventObject {
    private String mExecutionId;
    public LineStart(Object source) {
        super(source);
    }

    public LineStart(Object source, String executionId){
        super(source);
        mExecutionId = executionId;
    }
    public String getExecutionId(){
        return mExecutionId;
    }

    public String getEventDescription() {
        return "Line Started";
    }
}
