package de.dfki.vsm.xtension.charamel.xml.command.object.action.charamel;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.charamel.xml.command.object.action.CharamelActObject;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public class SetPosition extends CharamelActObject implements XMLParseable, XMLWriteable {

    private String mCharameAvatarId = "1";
    private int mXPos = 0;
    private int mYPos = 0;
    private int mZPos = 0;

    private int mXOrient = 0;
    private int mYOrient = 0;
    private int mZOrient = 0;

    public SetPosition(String aid, String xpos, String ypos, String zpos, String xorient, String yorient, String zorient) {
        mName = "caixml";
        mCharameAvatarId = aid;
        mXPos = Integer.parseInt(xpos);
        mYPos = Integer.parseInt(ypos);
        mZPos = Integer.parseInt(zpos);
        mXOrient = Integer.parseInt(xorient);
        mYOrient = Integer.parseInt(yorient);
        mZOrient = Integer.parseInt(zorient);
    }

    public SetPosition() {
        mName = "caixml";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();
        out.push().println("<cai_request version=\"1.0\">");
        out.push().println("<cai_command id=\"" + mId + "\">SetAvatarPositionXML");
        out.push().println("<position x=\"" + mXPos + "\" y=\"" + mYPos + "\" z=\"" + mZPos + "\"></position>");
        out.push().println("<orientation x=\"" + mXOrient + "\" y=\"" + mYOrient + "\" z=\"" + mZOrient + "\"></orientation>");
        out.pop().println("</cai_command>");
        out.pop().println("</cai_request>");
        out.pop().pop().println("</Action>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
    }
}
