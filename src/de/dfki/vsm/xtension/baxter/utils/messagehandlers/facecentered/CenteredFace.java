package de.dfki.vsm.xtension.baxter.utils.messagehandlers.facecentered;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.baxter.utils.MessageObservable;
import de.dfki.vsm.xtension.baxter.utils.MessageObserver;
import de.dfki.vsm.xtension.baxter.utils.communication.BaxterCommandSender;
import org.apache.commons.lang.StringUtils;

/**
 * Created by alvaro on 22.09.16.
 */
public class CenteredFace implements MessageObserver {
    private final RunTimeProject project;

    public CenteredFace(RunTimeProject project, MessageObservable observable) {
        this.project = project;
        observable.register(this);
    }

    @Override
    public void update(String message) {
        if(message.contains("#FACECENTERED#")){
            project.setVariable("LookingAtFace", true);
        }
        if(message.contains("#FACENONCENTERED#")){
            project.setVariable("LookingAtFace", false);
            int pos_start = StringUtils.ordinalIndexOf(message, "#", 2);
            int pos_end = StringUtils.ordinalIndexOf(message, "#", 3);
            if(pos_start > 0 && pos_end > 0) {
                String movement = message.substring(pos_start + 1, pos_end);
                try {
                    Float.parseFloat(movement);
                    BaxterCommandSender.BaxterLookFace(movement);
                }catch (NumberFormatException e){

                }
            }
        }
    }
}
