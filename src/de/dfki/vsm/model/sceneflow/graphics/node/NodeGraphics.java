package de.dfki.vsm.model.sceneflow.graphics.node;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class NodeGraphics implements ModelObject {

    // The node position
    private NodePosition mPosition;

    // Create the position
    public NodeGraphics() {
        mPosition = new NodePosition();
    }

    // Create the position
    public NodeGraphics(final NodePosition position) {
        mPosition = position;
    }

    // Create the position
    public NodeGraphics(final int xPos, final int yPos) {
        mPosition = new NodePosition(xPos, yPos);
    }

    // Get the position
    public final NodePosition getPosition() {
        return mPosition;
    }

    // Set te position
    public final void setPosition(final NodePosition value) {
        mPosition = value;
    }

    // Set the position
    public final void setPosition(final int xpos, final int ypos) {
        mPosition.setXPos(xpos);
        mPosition.setYPos(ypos);
    }

    @Override
    public final NodeGraphics getCopy() {
        return new NodeGraphics(mPosition.getCopy());
    }

    @Override
    public final void writeXML(IOSIndentWriter out) {
        out.println("<Graphics>").push();
        mPosition.writeXML(out);
        out.pop().println("</Graphics>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Position", new XMLParseAction() {
            @Override
            public void run(final Element element) {
                mPosition.parseXML(element);
            }
        });
    }
}
