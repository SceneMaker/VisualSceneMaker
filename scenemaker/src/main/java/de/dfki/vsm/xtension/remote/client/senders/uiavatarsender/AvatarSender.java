package de.dfki.vsm.xtension.remote.client.senders.uiavatarsender;

import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.util.LinkedList;

/**
 * Created by alvaro on 5/2/17.
 */
public abstract class AvatarSender implements DataSendable {

    public static final String QUIT_COMMAND = "QUIT";
    protected final String message;
    protected final String separator;
    protected final LinkedList<String> options;
    protected StringBuilder xmlBuilder;

    public AvatarSender(String message, String separator) {
        this.message = message;
        this.separator = separator;
        options = new LinkedList<>();
    }

    @Override
    public String buildDataToSent() {
        xmlBuilder = new StringBuilder();
        prepareData();
        openMessage();
        appendValues();
        closeMessage();
        return xmlBuilder.toString();
    }

    @Override
    public abstract void prepareData();

    private void closeMessage() {
        xmlBuilder.append("</messages>");
        xmlBuilder.append("\n");
    }

    private void openMessage() {
        xmlBuilder.append("<?xml version=\"1.0\"?>");
        xmlBuilder.append("<messages type=\"")
                .append(getSendingType())
                .append("\">");
    }

    private void appendValues() {
        xmlBuilder.append("<values>");
        for (String option : options) {
            xmlBuilder.append("<value>");
            xmlBuilder.append(option);
            xmlBuilder.append("</value>");
        }
        xmlBuilder.append("</values>");
    }

    protected abstract String getSendingType();

    public String buildCloseConnectionCommand(){
        return QUIT_COMMAND;
    }
}
