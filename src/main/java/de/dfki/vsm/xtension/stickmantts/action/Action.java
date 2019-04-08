/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.stickmantts.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 */
public class Action implements XMLParseable, XMLWriteable {

    protected String mName = "";
    protected String mId = "";
    
    public void setId(String id) {
        mId = id;
    }
    
    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
    }
}
