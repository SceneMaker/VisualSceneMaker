package de.dfki.vsm.xtension.remotesender.senders.uiavatarsender;

import de.dfki.vsm.xtension.remotesender.sender.DataSendable;

import java.util.Arrays;
import java.util.LinkedList;

/**
 * Created by alvaro on 5/2/17.
 */
public class OptionsSender implements DataSendable {
    public static final String MESSAGE_TYPE = "options";
    private final String message;
    private final String separator;
    private LinkedList<String> options = new LinkedList<>();
    private StringBuilder xmlBuilder;

    public OptionsSender(String message, String separator) {
        this.message = message;
        this.separator = separator;
    }

    @Override
    public void prepareData() {
        if(hasSeparator()){
            separateOptions();
        }else{
            options.add(message);
        }
    }

    private void separateOptions() {
        String [] messages = message.split(separator);
        options.addAll(Arrays.asList(messages));
    }

    private boolean hasSeparator() {
        return !separator.equals("");
    }

    @Override
    public String buildDataToSent() {
        prepareData();
        xmlBuilder = new StringBuilder();
        openMessage();
        appendValues();
        closeMessage();
        return xmlBuilder.toString();
    }



    private void closeMessage() {
        xmlBuilder.append("</messages>");
        xmlBuilder.append("\n");
    }

    private void openMessage() {
        xmlBuilder.append("<?xml version=\"1.0\"?>");
        xmlBuilder.append("<messages type=\"" + MESSAGE_TYPE + "\">");
    }

    private void appendValues() {
        xmlBuilder.append("<values>");
        for (String option:options  ) {
            xmlBuilder.append("<value>");
            xmlBuilder.append(option);
            xmlBuilder.append("</value>");
        }
        xmlBuilder.append("</values>");
    }
}
