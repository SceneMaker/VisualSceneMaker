package de.dfki.vsm.xtension.charamel.xml.feedback.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.charamel.CharamelExecutor;
import de.dfki.vsm.xtension.charamel.xml.feedback.object.Object;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Patrick Gebhard, Manuel Anglet
 *
 */
public class Feedback extends CharaXMLElement implements XMLParseable, XMLWriteable {

    public ArrayList<Action> mFeedbackActions = null;
    public ArrayList<Object> mFeedbackObjects = null;
    public ArrayList<Tts> mFeedbackTts = null;

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public Feedback() {
        mFeedbackActions = new ArrayList<>();
        mFeedbackObjects = new ArrayList<>();
        mFeedbackTts = new ArrayList<>();
    }

    Feedback(CharaXMLElement parent) {
        this.parent = parent;
    }

    public boolean hasActionFeedback() {
        return (mFeedbackActions != null) && !mFeedbackActions.isEmpty();
    }

    public boolean hasObjectFeedback() {
        return (mFeedbackObjects != null) && !mFeedbackObjects.isEmpty();
    }

    public boolean hasTtsFeedback() {
        return (mFeedbackTts != null) && !mFeedbackTts.isEmpty();
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

       String name = element.getTagName();
       mLogger.message("parsing charamel feedback with tag "+ name);
       CharaXMLElement child;
       switch (name){
                case "cai_event":
                    child = new CaiEvent(this);
                    try {
                        ((CaiEvent)child).parseXML(element);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing cai_event", ex);
                    }
                    children.add(child);
                    break;
                case "tts":
                    child = new Tts(this);
                    try {
                        ((Tts)child).parseXML(element);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing tts", ex);
                    }
                    children.add(child);
                    break;
                case "action":
                    child = new Action(this);
                    try {
                        ((Action)child).parseXML(element);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;
                case "status":
                    child = new Status(this);
                    try {
                        ((Status)child).parseXML(element);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;
                case "cai_command":
                    child = new CaiCommand(this);
                    try {
                        ((CaiCommand)child).parseXML(element);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;
                case "cai_response":
                    child = new CaiResponse(this);
                    try {
                        ((CaiResponse)child).parseXML(element);
                    } catch (XMLParseError ex) {
                        Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "error parsing action", ex);
                    }
                    children.add(child);
                    break;

                default:
                    Logger.getLogger(CharaXMLElement.class.getName()).log(Level.SEVERE, "Could not parse XML tag:{0}", name);
            }
    }
    @Override
    public void handle(CharamelExecutor executor) {executor.handle(this);}
}
