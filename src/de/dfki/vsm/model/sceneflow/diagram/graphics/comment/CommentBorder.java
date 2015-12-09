package de.dfki.vsm.model.sceneflow.diagram.graphics.comment;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.SyntaxObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class CommentBorder extends SyntaxObject {
    private int mXPos;
    private int mYPos;
    private int mWidth;
    private int mHeight;

    public CommentBorder() {
        mXPos   = Integer.MIN_VALUE;
        mYPos   = Integer.MIN_VALUE;
        mWidth  = Integer.MIN_VALUE;
        mHeight = Integer.MIN_VALUE;
    }

    public CommentBorder(int xPos, int yPos, int width, int height) {
        mXPos   = xPos;
        mYPos   = yPos;
        mWidth  = width;
        mHeight = height;
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

    public void setWidth(int value) {
        mWidth = value;
    }

    public int getWidth() {
        return mWidth;
    }

    public void setHeight(int value) {
        mHeight = value;
    }

    public int getHeight() {
        return mHeight;
    }

    public String getAbstractSyntax() {
        return "Rect(" + mXPos + "," + mYPos + "," + mWidth + "," + mHeight + ")";
    }

    public String getConcreteSyntax() {
        return getAbstractSyntax();
    }

    public String getFormattedSyntax() {
        return "";
    }

    public CommentBorder getCopy() {
        return new CommentBorder(mXPos, mYPos, mWidth, mHeight);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Rect x-pos=\"" + mXPos + "\" y-pos=\"" + mYPos + "\" width=\"" + mWidth + "\" height=\""
                    + mHeight + "\"/>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mXPos   = Integer.valueOf(element.getAttribute("x-pos"));
        mYPos   = Integer.valueOf(element.getAttribute("y-pos"));
        mWidth  = Integer.valueOf(element.getAttribute("width"));
        mHeight = Integer.valueOf(element.getAttribute("height"));
    }
}
