/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.charamel.xml.feedback.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import de.dfki.vsm.xtension.charamel.CharamelExecutor;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard, Manuel Anglet
 *
 */
public class CaiEvent extends CharaXMLElement implements XMLParseable, XMLWriteable {

    public Tts mTts = null;
    String id;
    String name;


    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public CaiEvent() {
    }

    CaiEvent(CharaXMLElement parent) {
        this.parent = parent;
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
        id = element.getAttribute("id");
        name = element.getAttribute("name");
        this.parseChildren(element);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public void handle(CharamelExecutor executor) {executor.handle(this);}
}
