package de.dfki.vsm.model.sceneflow.graphics.edge;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class EdgePoint implements ModelObject {

    private int mXPpos;
    private int mCtrlXPos;
    private int mYPos;
    private int mCtrlYPos;

    public EdgePoint() {
        mXPpos = Integer.MIN_VALUE;
        mYPos = Integer.MIN_VALUE;
        mCtrlXPos = Integer.MIN_VALUE;
        mCtrlYPos = Integer.MIN_VALUE;
    }

    public EdgePoint(int xPos, int ctrlXPos, int yPos, int ctrlYPos) {
        mXPpos = xPos;
        mYPos = yPos;
        mCtrlXPos = ctrlXPos;
        mCtrlYPos = ctrlYPos;
    }

    public final int getXPos() {
        return mXPpos;
    }

    public final void setXPos(final int value) {
        mXPpos = value;
    }

    public final int getCtrlXPos() {
        return mCtrlXPos;
    }

    public final void setCtrlXPos(final int value) {
        mCtrlXPos = value;
    }

    public final int getYPos() {
        return mYPos;
    }

    public final void setYPos(final int value) {
        mYPos = value;
    }

    public final int getCtrlYPos() {
        return mCtrlYPos;
    }

    public final void setCtrlYPos(final int value) {
        mCtrlYPos = value;
    }

    @Override
    public final EdgePoint getCopy() {
        return new EdgePoint(mXPpos, mCtrlXPos, mYPos, mCtrlYPos);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<ControlPoint "
                + "xPos=\"" + mXPpos + "\" "
                + "yPos=\"" + mYPos + "\" "
                + "ctrlXPos=\"" + mCtrlXPos + "\" "
                + "ctrlYPos=\"" + mCtrlYPos + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mXPpos = Integer.valueOf(element.getAttribute("xPos"));
        mYPos = Integer.valueOf(element.getAttribute("yPos"));
        mCtrlXPos = Integer.valueOf(element.getAttribute("ctrlXPos"));
        mCtrlYPos = Integer.valueOf(element.getAttribute("ctrlYPos"));
    }
}
