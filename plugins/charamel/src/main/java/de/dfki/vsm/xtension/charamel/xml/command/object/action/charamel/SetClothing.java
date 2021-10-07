/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.charamel.xml.command.object.action.charamel;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.charamel.xml.command.object.action.CharamelActObject;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public class SetClothing extends CharamelActObject implements XMLParseable, XMLWriteable {

    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private String mCharameAvatarId = "1";
    private String mCharacterClothing = "business_open_shortleg";

    public SetClothing(String aid, String clothing) {
        mName = "caixml";
        mCharameAvatarId = aid;
        mCharacterClothing = clothing;
    }

    public SetClothing() {
        mName = "caixml";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();
        out.push().println("<cai_request version=\"1.0\">");
        out.push().println("<cai_command>RenderXML");
        out.push().println("<animation_track>");
        out.push().println("<event aid='" + mCharameAvatarId + "'>SetClothing");
        out.println("<event_param>" + mCharacterClothing + "</event_param>");
        out.println("<event_param>1</event_param>");
        out.println("<event_param>default</event_param>");
        out.pop().println("</event>");
        out.pop().println("</animation_track>");
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
