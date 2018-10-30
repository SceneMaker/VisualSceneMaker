package de.dfki.vsm.xtension.remote.client.senders.uiavatarsender;

/**
 * Created by alvaro on 5/2/17.
 */
public class MoodSender extends AvatarSender {

    public static final String MOOD_OPTION = "mood";
    public MoodSender(String message, String separator) {
        super(message, separator);
    }

    @Override
    public void prepareData() {
        options.add(message);
    }

    @Override
    protected String getSendingType() {
        return MOOD_OPTION;
    }
}
