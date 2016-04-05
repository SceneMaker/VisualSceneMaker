/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.command;

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
public class TWorldCommandObject implements XMLParseable, XMLWriteable {

    String mName = "";
    TWorldCommandObjectAction mAction = null;

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public TWorldCommandObject(String name,TWorldCommandObjectAction a) {
        mName = name;
       mAction = a;
    }

    public TWorldCommandObject() {
    }

    public void setAction(TWorldCommandObjectAction a) {
        mAction = a;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Object name=\"" + mName + "\">").push();

        mAction.writeXML(out);

        out.pop().pop().println("</Object>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String name = element.getTagName();

                if (name.equalsIgnoreCase("Action")) {
                    String actionName = element.getAttribute("name");

                    if (actionName.equalsIgnoreCase("MovetoLocation")) {
                        mAction = new TWorldCommandObjectMoveToLoactionAction();
                        mAction.parseXML(element);
                    }

                    if (actionName.equalsIgnoreCase("ambient_setup")) {
                        mAction = new TWorldCommandObjectAmbientSetupAction();
                        mAction.parseXML(element);
                    }

                    if (actionName.equalsIgnoreCase("set_sound_ambient")) {
                        mAction = new TWorldCommandObjectSetSoundAmbientAction();
                        mAction.parseXML(element);
                    }
                    if (actionName.equalsIgnoreCase("caixml")) {
                        mAction = new TWorldCommandObjectCharamelAction();
                        mAction.parseXML(element);
                    }
                }
            }
        });
    }
}