package de.dfki.vsm.xtension.charamel.xml.feedback.action;

import de.dfki.vsm.xtension.charamel.xml.feedback.object.Object;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.charamel.CharamelExecutor;
import java.util.ArrayList;
import org.w3c.dom.Element;

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
    static final LOGConsoleLogger sLogger = LOGConsoleLogger.getInstance();

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
       this.parseChildren(element);
        /*
        
        String tag = element.getTagName();
        
        switch (tag) {
            case "cai_event":
                CaiEvent ce = new CaiEvent(this);
                ce.parseXML(element);
                
            
            case "action":
                Action fa = new Action(this);
                fa.parseXML(element);
                mFeedbackActions.add(fa);
                break;
                
            case "tts":
                Tts tts = mew Tts(this);
        }
        
         legacy solution jumped over cai-event 
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {

                final String name = element.getTagName();
                System.out.println("###Tag:" + name);
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
                
                if (name.equalsIgnoreCase("tts")) {
                    Tts ft = new Tts();

                    ft.parseXML(element);
                    mFeedbackTts.add(ft);
                }
            }
        });
        */
    }
    @Override
    public void handle(CharamelExecutor executor) {executor.handle(this);}
}
