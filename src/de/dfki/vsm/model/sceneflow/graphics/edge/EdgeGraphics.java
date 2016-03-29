package de.dfki.vsm.model.sceneflow.graphics.edge;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

// The edge graphics
public final class EdgeGraphics implements ModelObject {

    // The edge connection
    private EdgeArrow mConnection;

    // Create the graphics
    public EdgeGraphics() {
        mConnection = new EdgeArrow();
    }

    // Create the graphics
    public EdgeGraphics(final EdgeArrow connection) {
        mConnection = connection;
    }

    // Set the connection
    public void setConnection(final EdgeArrow value) {
        mConnection = value;
    }

    // Get the connection
    public EdgeArrow getConnection() {
        return mConnection;
    }

    @Override
    public final EdgeGraphics getCopy() {
        return new EdgeGraphics(mConnection.getCopy());
    }

    @Override
    public final void writeXML(IOSIndentWriter out) {
        out.println("<Graphics>").push();
        mConnection.writeXML(out);
        out.pop().println("</Graphics>");
    }

    @Override
    public final void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Connection", new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                mConnection.parseXML(element);
            }
        });
    }

    public final int getHashCode() {
        int hashCode = 0;

        for (EdgePoint a : mConnection.getPointList()) {
            hashCode += a.getCtrlXPos();
            hashCode += a.getCtrlYPos();
        }

        return hashCode;
    }
}
