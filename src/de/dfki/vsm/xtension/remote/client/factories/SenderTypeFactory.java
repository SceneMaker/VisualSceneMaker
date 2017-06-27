package de.dfki.vsm.xtension.remote.client.factories;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;
import de.dfki.vsm.xtension.remote.client.senders.StringDefaultSender;
import de.dfki.vsm.xtension.remote.client.senders.decorators.NewLineSendableDecorator;
import de.dfki.vsm.xtension.remote.client.senders.uiavatarsender.MoodSender;
import de.dfki.vsm.xtension.remote.client.senders.uiavatarsender.OptionsSender;

/**
 * Created by alvaro on 5/2/17.
 */
public class SenderTypeFactory {
    public DataSendable buildSendable(String type, String message, String separator) {
        DataSendable sendable;
        if(type.contains("optionsSender") && !separator.equals("")){
            sendable =  new OptionsSender(message, separator);
        }else if(type.contains("moodSender")){
            sendable =  new MoodSender(message, "");
        }else{
            sendable =  new StringDefaultSender(message);
        }
        return new NewLineSendableDecorator(sendable);
    }
}
