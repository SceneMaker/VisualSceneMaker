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
public class Color extends Action implements XMLParseable, XMLWriteable {

    String mRed = "";
    String mGreen = "";
    String mBlue = "";

    public Color(String red, String green, String blue) {
        mName = "color";
        mRed = red;
        mGreen = green;
        mBlue = blue;
    }

    public Color() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" r=\"" + mRed + "\" g=\"" + mGreen + "\" b=\"" + mBlue + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mRed = element.getAttribute("r");
        mGreen = element.getAttribute("g");
        mBlue = element.getAttribute("b");
        mId = element.getAttribute("id");
    }
}
