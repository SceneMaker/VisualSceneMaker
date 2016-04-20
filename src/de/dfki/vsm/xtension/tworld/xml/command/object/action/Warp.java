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
public class Warp extends Action implements XMLParseable, XMLWriteable {

    String mLocation = "";
    String mViewtarget = "";

    public Warp(String location) {
        mName = "warp";
        mLocation = location;
    }

    public Warp(String location, String target) {
        mName = "warp";
        mLocation = location;
        mViewtarget = target;
    }

    public Warp() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" locname=\"" + mLocation + "\"" + ((!mViewtarget.equalsIgnoreCase("")) ? " viewtarget=\"" + mViewtarget + "\"" : "") + "/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mLocation = element.getAttribute("locname");
        mViewtarget = element.getAttribute("viewtarget");
        mViewtarget = (mViewtarget == null) ? "" : mViewtarget;
        mId = element.getAttribute("id");
    }
}
