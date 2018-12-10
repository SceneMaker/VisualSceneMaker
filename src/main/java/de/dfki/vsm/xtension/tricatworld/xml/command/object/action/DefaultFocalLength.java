/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tricatworld.xml.command.object.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class DefaultFocalLength extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

   
    String mBlendTime = "";

    public DefaultFocalLength(String blendtime) {
        mName = "setfieldofview_default";

        mBlendTime = blendtime;
    }

    public DefaultFocalLength() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" blendtime=\"" + mBlendTime + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mBlendTime = element.getAttribute("blendtime");
        mId = element.getAttribute("id");
    }
}
