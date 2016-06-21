/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.command.object.action;

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
public class WorldPosition extends Action implements XMLParseable, XMLWriteable {

    String mX = "";
    String mY = "";
    String mZ = "";

    public WorldPosition(String x, String y, String z) {
        mName = "warptoworldposition";
        mX = x;
        mY = y;
        mZ = z;
    }

    public WorldPosition() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" x=\"" + mX + "\" y=\"" + mY + "\" z=\"" + mZ + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mX = element.getAttribute("x");
        mY = element.getAttribute("y");
        mZ = element.getAttribute("z");
        mId = element.getAttribute("id");
    }
}
