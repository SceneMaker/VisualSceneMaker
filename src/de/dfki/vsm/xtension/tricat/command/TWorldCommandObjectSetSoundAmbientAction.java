/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tricat.command;

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
public class TWorldCommandObjectSetSoundAmbientAction extends TWorldCommandObjectAction implements XMLParseable, XMLWriteable {

    String mValue = "";

    public TWorldCommandObjectSetSoundAmbientAction(String value) {
        mName = "set_sound_ambient";
        mValue = value;
    }

    public TWorldCommandObjectSetSoundAmbientAction() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + " id=\"" + mId + "\" value=\"" + mValue + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mValue = element.getAttribute("value");
        mId = element.getAttribute("id");
    }
}
