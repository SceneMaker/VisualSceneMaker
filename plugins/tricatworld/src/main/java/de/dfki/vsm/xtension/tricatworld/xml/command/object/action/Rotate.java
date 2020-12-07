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
public class Rotate extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private boolean mdetailedRotation = false;
    String mDegree = "";
    String mRotHor = "";
    String mRotVer = "";

    public Rotate(String value) {
        mName = "setworldrotation";
        mDegree = value;
    }

    public Rotate(String h, String v) {
        mName = "setworldrotation";
        mRotHor = h;
        mRotVer = v;
        mdetailedRotation = true;
    }

    public Rotate() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {

        if (!mdetailedRotation) {
            out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" deg=\"" + mDegree + "\"/>");
        } else {
            out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" yaw=\"" + mRotHor + "\" pitch=\"" + mRotVer + "\" snap=\"0\"/>");
        }
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");

        mId = element.getAttribute("id");

        if ((element.hasAttribute("yaw")) && (element.hasAttribute("pitch"))) {
            mdetailedRotation = true;
            mRotHor = element.getAttribute("yaw");
            mRotVer = element.getAttribute("pitch");
        }

        if (element.hasAttribute("deg")) {
            mdetailedRotation = true;
            mDegree = element.getAttribute("deg");
        }
    }
}
