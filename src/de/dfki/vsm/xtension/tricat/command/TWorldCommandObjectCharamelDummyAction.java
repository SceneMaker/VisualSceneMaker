/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tricat.command;

import de.dfki.vsm.util.ios.IOSIndentWriter;
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
public class TWorldCommandObjectCharamelDummyAction extends TWorldCommandObjectAction implements XMLParseable, XMLWriteable {

    // TODO cai_request sub element String mValue = "";
    public TWorldCommandObjectCharamelDummyAction(String value) {
        mName = "caixml";
        // TODO add cai_request elementmValue = value;
    }

    public TWorldCommandObjectCharamelDummyAction() {
         mName = "caixml";
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + " id=\"" + mId + "\"").push();

        out.push().println("<cai_request version='1.0'>");
        out.push().println("<cai_command id=\"2\">RenderXML");
        out.push().println("<animation_track>");
        out.println("<pause></pause>");
        out.pop().println("<motion speed='1.0' attack='1000' decay='1000' start='0' duration='9999'>walk/turns/turn_90r</motion>");
        out.pop().println("</animation_track>");
        out.pop().println("</cai_command>");
        out.pop().println("</cai_request>");
        out.pop().println("</Action>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
