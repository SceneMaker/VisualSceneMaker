package de.dfki.vsm.xtension.charamel.xml.feedback;

import de.dfki.vsm.xtension.charamel.xml.feedback.object.Object;
import de.dfki.vsm.xtension.charamel.xml.feedback.action.Action;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class CharamelFeedback implements XMLParseable, XMLWriteable {

    public ArrayList<Action> mFeedbackActions = null;
    public ArrayList<Object> mFeedbackObjects = null;

    // Logger
    static final LOGConsoleLogger sLogger = LOGConsoleLogger.getInstance();

    public CharamelFeedback() {
        mFeedbackActions = new ArrayList<>();
        mFeedbackObjects = new ArrayList<>();
    }

    public boolean hasActionFeedback() {
        return (mFeedbackActions != null) && !mFeedbackActions.isEmpty();
    }

    public boolean hasObjectFeedback() {
        return (mFeedbackObjects != null) && !mFeedbackObjects.isEmpty();
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
                    mFeedbackActions.add(fa);
                }

                if (name.equalsIgnoreCase("object")) {
                    Object fo = new Object();

                    fo.parseXML(element);
                    mFeedbackObjects.add(fo);
                }
            }
        });
    }
}
