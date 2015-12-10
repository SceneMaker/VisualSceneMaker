package de.dfki.vsm.model.sceneflow.diagram.graphics.node;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/** 
 * A position of a node
 *
 * @author Not me
 */
public class NodePosition implements ModelObject {
    private int mXPos;
    private int mYPos;

    public NodePosition() {
        mXPos = Integer.MIN_VALUE;
        mYPos = Integer.MIN_VALUE;
    }

    public NodePosition(int xPos, int yPos) {
        mXPos = xPos;
        mYPos = yPos;
    }

    public void setXPos(int value) {
        mXPos = value;
    }

    public int getXPos() {
        return mXPos;
    }

    public void setYPos(int value) {
        mYPos = value;
    }

    public int getYPos() {
        return mYPos;
    }

//    public String getAbstractSyntax() {
//        return "Position(" + mXPos + "," + mYPos + ")";
//    }
//
//    public String getConcreteSyntax() {
//        return getAbstractSyntax();
//    }
//
//    public String getFormattedSyntax() {
//        return "";
//    }

    @Override
    public NodePosition getCopy() {
        return new NodePosition(mXPos, mYPos);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Position x-pos=\"" + mXPos + "\" y-pos=\"" + mYPos + "\"/>");
    }

    @Override
    public void parseXML(Element element) {
        mXPos = Integer.valueOf(element.getAttribute("x-pos"));
        mYPos = Integer.valueOf(element.getAttribute("y-pos"));
    }
}
