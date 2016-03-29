package de.dfki.vsm.model.sceneflow.graphics.node;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class NodePosition implements ModelObject {

    // The Y coordinate
    private int mXPos;
    // The Y coordinate
    private int mYPos;

    // Create a node position
    public NodePosition() {
        mXPos = Integer.MIN_VALUE;
        mYPos = Integer.MIN_VALUE;
    }

    // Create a node position
    public NodePosition(final int xPos, final int yPos) {
        mXPos = xPos;
        mYPos = yPos;
    }

    // Set the X coordinate
    public final void setXPos(final int value) {
        mXPos = value;
    }

    // Get the X coordinate
    public final int getXPos() {
        return mXPos;
    }

    // Set the Y coordinate
    public final void setYPos(final int value) {
        mYPos = value;
    }

    // Get the Y coordinate
    public final int getYPos() {
        return mYPos;
    }

    @Override
    public final NodePosition getCopy() {
        return new NodePosition(mXPos, mYPos);
    }

    @Override
    public final void writeXML(IOSIndentWriter out) {
        out.println("<Position xPos=\"" + mXPos + "\" yPos=\"" + mYPos + "\"/>");
    }

    @Override
    public final void parseXML(Element element) {
        mXPos = Integer.valueOf(element.getAttribute("xPos"));
        mYPos = Integer.valueOf(element.getAttribute("yPos"));
    }
}
