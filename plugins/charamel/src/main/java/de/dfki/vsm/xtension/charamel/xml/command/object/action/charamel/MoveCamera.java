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
public class MoveCamera extends CharamelActObject implements XMLParseable, XMLWriteable {

    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private final String posX;
    private final String posY;
    private final String posZ;
    private String lookX;
    private String lookZ;
    private String lookY;
    private String upX;
    private String upY;
    private String upZ;
    private String mCharameAvatarId = "1";

    // TODO cai_request sub element String mValue = "";
    public MoveCamera(String posX, String posY, String posZ, String lookX, String lookY, String lookZ, String upX, String upY, String upZ, String aid) {
        mName = "caixml";
        this.posX = posX;
        this.posY = posY;
        this.posZ = posZ;
        this.lookX = lookX;
        this.lookY = lookY;
        this.lookZ = lookZ;
        this.upX = upX;
        this.upY = upY;
        this.upZ = upZ;
        mCharameAvatarId = aid;
    }

    public MoveCamera() {
        mName = "caixml";
        posX = "0";
        posY = "0";
        posZ = "0";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();


        String xml = "<cai_request version=\"1.0\">"
                + "<cai_command id=\"" + mId + "\">SetCameraXML"
                + "<position x=\"" + posX + "\" y=\"" + posY + "\" z=\"" + posZ + "\"></position>"
                + "<look_at x=\"" + lookX + "\" y=\"" + lookY + "\" z=\"" + lookZ + "\"></look_at>"
                + "<up_vector x=\"" + upX + "\" y=\"" + upY + "\" z=\"" + upZ + "\"></up_vector>"
                + "</cai_command></cai_request>";
        // + "<name mode=\"set\">VSMCamera</name> "
        out.push().println(xml);
        //out.println(xml);
        out.pop().pop().println("</Action>");
    }


    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
