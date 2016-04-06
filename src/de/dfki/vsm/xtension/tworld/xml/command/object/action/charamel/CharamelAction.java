/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.command.object.action.charamel;

import de.dfki.charactor.v4235.AnimationTrack;
import de.dfki.charactor.v4235.ComplexAnimationGenerator;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.Action;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class CharamelAction extends Action implements XMLParseable, XMLWriteable {

    private final String mCharamelCmd;
    private final int mCharamelCharacterId = 2;
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // TODO cai_request sub element String mValue = "";
    public CharamelAction(String value) {
        mName = "caixml";
        mCharamelCmd = value;
    }

    public CharamelAction() {
        mName = "caixml";
        mCharamelCmd = "";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();

        if (mCharamelCmd.equalsIgnoreCase("Speak")) {
            ComplexAnimationGenerator ca = new ComplexAnimationGenerator();
            AnimationTrack track1 = ca.addTrack();
            track1.addSpeakText(mCharamelCharacterId, "Hallo, ich bin die Susanne!");

            out.push().println(ca.getCaiXML());
            //mCP.close();
        }

//        out.push().println("<cai_request version='1.0'>");
//        out.push().println("<cai_command id=\"2\">RenderXML");
//        out.push().println("<animation_track>");
//        out.println("<pause></pause>");
//        out.pop().println("<motion speed='1.0' attack='1000' decay='1000' start='0' duration='9999'>walk/turns/turn_90r</motion>");
//        out.pop().println("</animation_track>");
//        out.pop().println("</cai_command>");
//        out.pop().println("</cai_request>");

        out.pop().println("</Action>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
