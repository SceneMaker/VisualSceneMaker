/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tricatworld.xml.command.object.action.charamel;

import de.dfki.iui.libcharamel.v4235.AnimationTrack;
import de.dfki.iui.libcharamel.v4235.ComplexAnimationGenerator;
import de.dfki.iui.libcharamel.v4235.Morph;
import de.dfki.iui.libcharamel.v4235.Motion;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.TriCatWorldActObject;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Angry extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private String mIntentsity;
    private String mCharameAvatarId = "1";
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // TODO cai_request sub element String mValue = "";
    public Angry(String intensity, String aid) {
        mName = "caixml";
        mIntentsity = intensity;
        mCharameAvatarId = aid;
    }

    public Angry() {
        mName = "caixml";
        mIntentsity = "1.0";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();

        ComplexAnimationGenerator ca = new ComplexAnimationGenerator();
        ca.getCommand().setId(mId); // set the same id in the Charamel command that has been used in the Tworld command
        ca.getCommand().setAid(Integer.parseInt(mCharameAvatarId));
        AnimationTrack track1 = ca.addTrack();
        track1.morph(Integer.parseInt(mCharameAvatarId), Float.parseFloat(mIntentsity), 3000, Morph.Angry);
        out.push().println(ca.getCaiXML());
        out.pop().pop().println("</Action>");
    }


    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
