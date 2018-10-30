package de.dfki.vsm.xtension.voicerecognition.plugins.sender;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

/**
 * Created by alvaro on 6/20/17.
 */
public class VRSendable implements DataSendable {
    private final String spokenText;
    private final String messageId;
    private StringBuilder xmlBuilder;

    public VRSendable(String spokenText, String messageId) {
        this.spokenText = spokenText;
        this.messageId = messageId;
        xmlBuilder = new StringBuilder();
    }

    @Override
    public String buildDataToSent() {
        prepareData();
        return xmlBuilder.toString();
    }

    @Override
    public void prepareData() {
        xmlBuilder.append("<messages>")
                .append("<message type=\"voice_recognition\" id=\"" + messageId + "\">")
                .append(spokenText)
                .append("</message>")
                .append("</messages>")
        ;

    }

    @Override
    public String buildCloseConnectionCommand() {
        return null;
    }
}
