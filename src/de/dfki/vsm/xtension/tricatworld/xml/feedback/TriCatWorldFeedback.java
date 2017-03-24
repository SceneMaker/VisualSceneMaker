package de.dfki.vsm.xtension.tricatworld.xml.feedback;

import de.dfki.vsm.xtension.tricatworld.xml.feedback.object.Object;
import de.dfki.vsm.xtension.tricatworld.xml.feedback.action.Action;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public final class TriCatWorldFeedback implements XMLParseable, XMLWriteable {

    public Action mFeedbackAction = null;
    public Object mFeedbackObject = null;

    // Logger
    static final LOGConsoleLogger sLogger = LOGConsoleLogger.getInstance();

    public TriCatWorldFeedback() {
    }

    public boolean hasActionFeedback() {
        return (mFeedbackAction != null);
    }

    public boolean hasObjectFeedback() {
        return (mFeedbackObject != null);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<TWorldFeedback>").push();

//        mObjects.stream().forEach((o) -> {
//            try {
//                o.writeXML(out);
//            } catch (XMLWriteError ex) {
//                mLogger.failure(ex.getMessage());
//            }
//        });

        out.pop().println("</TWorldFeedback>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {

                final String name = element.getTagName();

                if (name.equalsIgnoreCase("action")) {
                    Action fa = new Action();

                    fa.parseXML(element);
                    mFeedbackAction = fa;
                }

                if (name.equalsIgnoreCase("object")) {
                    Object fo = new Object();

                    fo.parseXML(element);
                    mFeedbackObject = fo;
                }
            }
        });
    }
}
