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
public class HandToFace extends CharamelActObject implements XMLParseable, XMLWriteable {

    private String mCharameAvatarId = "1";
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // TODO cai_request sub element String mValue = "";
    public HandToFace(String aid) {
        mName = "caixml";
        mCharameAvatarId = aid;
    }

    public HandToFace() {
        mName = "caixml";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {

        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();

        String xml = "<cai_request version=\"1.0\">"
                + "<cai_command id=\"" + mId + "\">RenderXML"
                + "<animation_track>"
                + "<event aid=\"" + mCharameAvatarId + "\">"
                + "Motion<event_param>interaction/smell/stinky_01</event_param>"
                + "</event></animation_track>"
                + "</cai_command></cai_request>";
        out.push().println(xml);
        out.pop().pop().println("</Action>");


    }


    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
