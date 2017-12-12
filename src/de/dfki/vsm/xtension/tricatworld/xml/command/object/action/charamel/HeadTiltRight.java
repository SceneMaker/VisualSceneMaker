/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tricatworld.xml.command.object.action.charamel;

import de.dfki.iui.libcharamel.v4235.AnimationTrack;
import de.dfki.iui.libcharamel.v4235.ComplexAnimationGenerator;
import de.dfki.iui.libcharamel.v4235.Morph;
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
public class HeadTiltRight extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private String mIntentsity;
    private String mCharameAvatarId = "1";
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // TODO cai_request sub element String mValue = "";
    public HeadTiltRight(String intensity, String aid) {
        mName = "caixml";
        mIntentsity = intensity;
        mCharameAvatarId = aid;
    }

    public HeadTiltRight() {
        mName = "caixml";
        mIntentsity = "1.0";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();
/**
        ComplexAnimationGenerator ca = new ComplexAnimationGenerator();
        ca.getCommand().setId(mId); // set the same id in the Charamel command that has been used in the Tworld command
        ca.getCommand().setAid(Integer.parseInt(mCharameAvatarId));
        AnimationTrack track1 = ca.addTrack();
        track1.morph(Integer.parseInt(mCharameAvatarId), Float.parseFloat(mIntentsity), 3000, Morph.Happy);
        out.push().println(ca.getCaiXML());
        out.pop().pop().println("</Action>");
        
        
        System.out.println(ca.getCaiXML());  */
        
        //float deg = MAX_TILT_DEGREE * Float.parseFloat(mIntentsity);
        float orientation_x = 200;
        float orientation_y = 200;
        float orientation_z = -200;
        
        String xml = "<cai_request version=\"1.0\">"
                + "<cai_command id=\"" + mId + "\">RenderXML<animation_track>"
                + "<pause aid=\"" + Integer.parseInt(mCharameAvatarId) + "\">0</pause>"
                + "<event aid=\"" + Integer.parseInt(mCharameAvatarId) + "\">AddAnimationChannelXML<event_param><![CDATA[<cai_command>AddAnimationChannelXML<channel>JOINT_ORIENTATION_EULER<joint>HEAD</joint></channel></cai_command>]]></event_param>"
                + "</event>"
                + "<event aid=\"" + Integer.parseInt(mCharameAvatarId) + "\">SetAnimationChannelFactorXML<event_param><![CDATA[<cai_command>SetAnimationChannelFactorXML<interpolation>500</interpolation><channel>MOTION_CHANNEL<joint "
                + "factor=\"1.0\">HEAD</joint></channel><channel>JOINT_ORIENTATION_EULER<joint "
                + "factor=\"1.0\">HEAD</joint></channel></cai_command>]]></event_param>"
                + "</event>"
                + "<event aid=\"" + Integer.parseInt(mCharameAvatarId) + "\">SetAnimationChannelValuesXML<event_param><![CDATA[<cai_command>SetAnimationChannelValuesXML<interpolation>1000</interpolation><channel>JOINT_ORIENTATION_EULER<joint "
                + "orientation_x=\"" + orientation_x + "\" orientation_y=\"" + orientation_y + "\""
                + "orientation_z=\"" + orientation_z + "\">HEAD</joint></channel></cai_command>]]></event_param>"
                + "</event>"
                + "</animation_track>"
                + "</cai_command>"
                + "</cai_request>";
        out.push().println(xml);
        //out.println(xml);
        out.pop().pop().println("</Action>");
        
        
        System.out.println("???????????????????????????????????????????????????????????????????????????????????");      
        System.out.println(xml);
    }


    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
