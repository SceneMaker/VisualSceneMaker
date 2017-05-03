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
public class AddNote extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private String mText = "";
    private String mXPos = "";
    private String mYPos = "";

    public AddNote(String text, String xpos, String ypos) {
        mName = "addnote";
        mText = text;
        mXPos = xpos;
        mYPos = ypos;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" text=\"" + mText + "\" anchorx=\"" + mXPos + "\" anchory=\"" + mYPos + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        mText = element.getAttribute("text");
        mXPos = element.getAttribute("anchorx");
        mYPos = element.getAttribute("anchory");
    }
}
