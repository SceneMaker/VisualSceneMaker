package de.dfki.vsm.model.sceneflow.graphics.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A point with control point.
 *
 * @author Not me
 */
public class Point extends Syntax {
    private int mXPpos;
    private int mCtrlXPos;
    private int mYPos;
    private int mCtrlYPos;

    public Point() {
        mXPpos    = Integer.MIN_VALUE;
        mYPos     = Integer.MIN_VALUE;
        mCtrlXPos = Integer.MIN_VALUE;
        mCtrlYPos = Integer.MIN_VALUE;
    }

    public Point(int xpos, int ctrl_xpos, int ypos, int ctrl_ypos) {
        mXPpos    = xpos;
        mYPos     = ypos;
        mCtrlXPos = ctrl_xpos;
        mCtrlYPos = ctrl_ypos;
    }

    public int getXPos() {
        return mXPpos;
    }

    public void setXPos(int value) {
        mXPpos = value;
    }

    public int getCtrlXPos() {
        return mCtrlXPos;
    }

    public void setCtrlXPos(int value) {
        mCtrlXPos = value;
    }

    public int getYPos() {
        return mYPos;
    }

    public void setYPos(int value) {
        mYPos = value;
    }

    public int getCtrlYPos() {
        return mCtrlYPos;
    }

    public void setCtrlYPos(int value) {
        mCtrlYPos = value;
    }

    public String getAbstractSyntax() {
        return "Point(" + mYPos + "," + mYPos + "," + mCtrlXPos + "," + mCtrlYPos + ")";
    }

    public String getConcreteSyntax() {
        return getAbstractSyntax();
    }

    public String getFormattedSyntax() {
        return "";
    }

    public Point getCopy() {
        return new Point(mXPpos, mCtrlXPos, mYPos, mCtrlYPos);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Point x-pos=\"" + mXPpos + "\" y-pos=\"" + mYPos + "\" control-x-pos=\"" + mCtrlXPos
                    + "\" control-y-pos=\"" + mCtrlYPos + "\"/>");
    }

    public void parseXML(Element element) {
        mXPpos    = Integer.valueOf(element.getAttribute("x-pos"));
        mYPos     = Integer.valueOf(element.getAttribute("y-pos"));
        mCtrlXPos = Integer.valueOf(element.getAttribute("control-x-pos"));
        mCtrlYPos = Integer.valueOf(element.getAttribute("control-y-pos"));
    }
}
