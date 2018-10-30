package de.dfki.vsm.xtension.tricatworld.xml.command.object.action.charamel;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.TriCatWorldActObject;
import java.util.LinkedList;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public class Speak extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private LinkedList mTextBlocks;
    private String mPunctuation;
    private String mCharameAvatarId = "1";
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // TODO cai_request sub element String mValue = "";
    public Speak(LinkedList textblocks, String punct, String aid) {
        mName = "caixml";
        mTextBlocks = textblocks;
        mPunctuation = punct;
        mCharameAvatarId = aid;
    }

    public Speak() {
        mName = "caixml";
        mTextBlocks = new LinkedList();
        mPunctuation = "";
    }

    private String buildUtterance() {
        final StringBuilder builder = new StringBuilder();

        for (final Object item : mTextBlocks) {
            //String word = item.toString();
            String word = (String) item;
            builder.append(word);
            if (!item.equals(mTextBlocks.getLast())) {
                builder.append(' ');
            } else {
                builder.append(mPunctuation);
            }
        }
        return builder.toString();
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();
        // CDATA STUFF
        String xml = "<cai_request version=\"1.0\">"
                + "<cai_command aid=\"" + Integer.parseInt(mCharameAvatarId) + "\" id=\"" + mId + "\">"
                + "RenderXML"
                + "<animation_track>"
                + "<speak_text aid=\"" + Integer.parseInt(mCharameAvatarId) + "\">"
                + "<![CDATA["
                + buildUtterance()
                + "]]>"
                + "</speak_text>"
                + "</animation_track>"
                + "</cai_command>"
                + "</cai_request>";
        out.println(xml);

        /*API STUFF
        final ComplexAnimationGenerator generator = new ComplexAnimationGenerator();
        generator.getCommand().setId(mId); // set the same id in the Charamel command that has been used in the Tworld command
        generator.getCommand().setAid(Integer.parseInt(mCharameAvatarId));
        final AnimationTrack track = generator.addTrack();
        track.addSpeakText(Integer.parseInt(mCharameAvatarId), buildUtterance());
        out.push().println(generator.getCaiXML());
         */
        out.pop().println("</Action>");

    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
