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
public class LoadDebriefing extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private String mEventLog = "";
    private String mScreenVideo = "";
    private String mCameraVideo = "";

    public LoadDebriefing(String eventlog, String screenvideo, String cameravideo) {
        mName = "load";
        mEventLog = eventlog.replaceAll("\\'", "");
        mScreenVideo = screenvideo.replaceAll("\\'", "");
        mCameraVideo = cameravideo.replaceAll("\\'", "");
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" eventlog=\"" + mEventLog + "\" screenvideo=\"" + mScreenVideo + "\" cameravideo=\"" + mCameraVideo + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        mEventLog = element.getAttribute("eventlog");
        mScreenVideo = element.getAttribute("screenvideo");
        mCameraVideo = element.getAttribute("cameravideo");
    }
}
