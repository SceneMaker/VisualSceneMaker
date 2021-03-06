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
 * @author Patrick Gebhard, Manuel Anglet
 */
public class CaiCommand extends CharaXMLElement implements XMLParseable, XMLWriteable {

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    public String mId = "";

    public CaiCommand(String id) {
        mId = id;
    }

    public CaiCommand() {
    }

    /**
     * @param parent
     */
    public CaiCommand(CharaXMLElement parent) {
        this.parent = parent;
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mId = element.getAttribute("id");
        this.parseChildren(element);
    }

    @Override
    public void writeXML(IOSIndentWriter writer) throws XMLWriteError {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void handle(CharamelExecutor executor) {
        executor.handle(this);
    }

    public String getMId() {
        return mId;
    }
}