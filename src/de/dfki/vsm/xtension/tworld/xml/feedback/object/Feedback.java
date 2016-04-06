/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.feedback.object;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
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
public class Feedback implements XMLParseable, XMLWriteable {

    public String mName = "";
    public String mValue = "";
    public String mTriggerObject = "";

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public Feedback() {
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<feedback>").push();

//        mObjects.stream().forEach((o) -> {
//            try {
//                o.writeXML(out);
//            } catch (XMLWriteError ex) {
//                mLogger.failure(ex.getMessage());
//            }
//        });
        out.pop().println("</feedback>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mValue = element.getAttribute("value");
        mTriggerObject = element.getAttribute("trigger_object");

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
