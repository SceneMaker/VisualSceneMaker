/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.feedback.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
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
public class CaiEvent implements XMLParseable, XMLWriteable {

    public Tts mTts = null;

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public CaiEvent() {
    }

    public boolean hasTTSStatus() {
        return (mTts != null);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<cai_event>").push();

//        mObjects.stream().forEach((o) -> {
//            try {
//                o.writeXML(out);
//            } catch (XMLWriteError ex) {
//                mLogger.failure(ex.getMessage());
//            }
//        });
        out.pop().println("</cai_event>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {

                final String name = element.getTagName();

                if (name.equalsIgnoreCase("tts")) {

                    Tts t = new Tts();

                    t.parseXML(element);

                    mTts = t;
                }
            }
        });
    }
}
