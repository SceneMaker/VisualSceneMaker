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
public class TWorldCommandObjectMoveToLoactionAction extends TWorldCommandObjectAction implements XMLParseable, XMLWriteable {

    String mLocation = "";

    public TWorldCommandObjectMoveToLoactionAction(String location) {
        mName = "MoveToLocation";
        mLocation = location;
    }

    public TWorldCommandObjectMoveToLoactionAction() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + " id=\"" + mId + "\" locname=\"" + mLocation + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mLocation = element.getAttribute("locname");
        mId = element.getAttribute("id");
    }
}
