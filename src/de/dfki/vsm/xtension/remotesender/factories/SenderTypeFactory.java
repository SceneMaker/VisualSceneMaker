package de.dfki.vsm.xtension.remotesender.factories;

import de.dfki.vsm.xtension.remotesender.sender.DataSendable;
import de.dfki.vsm.xtension.remotesender.senders.StringDefaultSender;
import de.dfki.vsm.xtension.remotesender.senders.uiavatarsender.MoodSender;
import de.dfki.vsm.xtension.remotesender.senders.uiavatarsender.OptionsSender;

/**
 * Created by alvaro on 5/2/17.
 */
public class SenderTypeFactory {
    public DataSendable buildSendable(String type, String message, String separator) {
        if(type.contains("optionsSender") && !separator.equals("")){
            return new OptionsSender(message, separator);
        }else if(type.contains("moodSender")){
            return new MoodSender(message, "");
        }
        return new StringDefaultSender(message);
    }
}
