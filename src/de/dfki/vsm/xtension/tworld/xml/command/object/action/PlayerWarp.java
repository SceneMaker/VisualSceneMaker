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
public class PlayerWarp extends Action implements XMLParseable, XMLWriteable{
  
    String mValue = "";
    String mViewtarget = "";

    public PlayerWarp(String value) {
        mName = "player_warp";
        mValue = value;
        mViewtarget = mValue;
    }

    public PlayerWarp() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" locname=\"" + mValue + "\" viewtarget=\"" + mViewtarget + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mValue = element.getAttribute("locname");
        mViewtarget = element.getAttribute("viewtarget");
        mId = element.getAttribute("id");
    }  
}