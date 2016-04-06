/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.command;

import de.dfki.vsm.xtension.tworld.xml.command.object.Object;
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
public class TWorldCommand implements XMLParseable, XMLWriteable {

    public ArrayList<Object> mObjects = new ArrayList<>();

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public TWorldCommand() {

    }

    public void addObject(Object o) {
        mObjects.add(o);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<TWorldCommand>").push();

        mObjects.stream().forEach((o) -> {
            try {
                o.writeXML(out);
            } catch (XMLWriteError ex) {
                mLogger.failure(ex.getMessage());
            }
        });

        out.pop().println("</TWorldCommand>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {

                final String name = element.getTagName();

                if (name.equalsIgnoreCase("Object")) {
                    Object o = new Object();

                    o.parseXML(element);

                    mObjects.add(o);
                }
            }
        });
    }
}
