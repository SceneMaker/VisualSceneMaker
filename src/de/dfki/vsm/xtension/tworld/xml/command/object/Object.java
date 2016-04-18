/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.command.object;

import de.dfki.vsm.xtension.tworld.xml.command.object.action.MoveTo;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.SoundAmbient;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.charamel.CharamelAction;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.AmbientLight;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.Action;
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
public class Object implements XMLParseable, XMLWriteable {

    String mName = "";
    Action mAction = null;

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public Object(String name,Action a) {
        mName = name;
       mAction = a;
    }

    public Object() {
    }

    public void setAction(Action a) {
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
                        mAction = new MoveTo();
                        mAction.parseXML(element);
                    }

                    if (actionName.equalsIgnoreCase("ambient_setup")) {
                        mAction = new AmbientLight();
                        mAction.parseXML(element);
                    }

                    if (actionName.equalsIgnoreCase("set_sound_ambient")) {
                        mAction = new SoundAmbient();
                        mAction.parseXML(element);
                    }
                    if (actionName.equalsIgnoreCase("caixml")) {
                        mAction = new CharamelAction();
                        mAction.parseXML(element);
                    }
                }
            }
        });
    }
}