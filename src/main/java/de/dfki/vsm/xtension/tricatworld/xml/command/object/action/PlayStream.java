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
public class PlayStream extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private String mPath = "";
    private String mStarttime = "";
    private String mEndtime = "";

    public PlayStream(String path, String starttime, String endtime) {
        mName = "playstream";
        mPath = path;
        mStarttime = starttime;
        mEndtime = endtime;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" path=\"" + mPath + "\" starttime=\"" + mStarttime + "\" endtime=\"" + mEndtime + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        mPath = element.getAttribute("path");
        mStarttime = element.getAttribute("starttime");
        mEndtime = element.getAttribute("endtime");
    }
}
