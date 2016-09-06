/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.feedback.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Tts implements XMLParseable, XMLWriteable {

    public String mStatus = "";
    public String mMarker = "";

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public Tts() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<marytts status>").push();

//        mObjects.stream().forEach((o) -> {
//            try {
//                o.writeXML(out);
//            } catch (XMLWriteError ex) {
//                mLogger.failure(ex.getMessage());
//            }
//        });
        out.pop().println("</marytts status>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mStatus = element.getAttribute("status");

        // get the marker - if there is one
        if (mStatus.equalsIgnoreCase("text_maker") || mStatus.equalsIgnoreCase("text_marker")) {
            mMarker = element.getFirstChild().getTextContent();

        }

//        // Process The Child Nodes
//        XMLParseAction.processChildNodes(element, new XMLParseAction() {
//            @Override
//            public void run(final Element element) throws XMLParseError {
//
//                final String name = element.getTagName();
//
//                if (name.equalsIgnoreCase("action")) {
//                    TWorldFeedbackAction fa = new TWorldFeedbackAction();
//
//                    fa.parseXML(element);
//
//                    mFeedbackAction = fa;
//                }
//            }
//        });
    }
}
