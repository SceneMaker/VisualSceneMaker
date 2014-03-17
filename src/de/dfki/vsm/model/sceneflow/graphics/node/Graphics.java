package de.dfki.vsm.model.sceneflow.graphics.node;

import de.dfki.vsm.model.sceneflow.Object;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class Graphics extends Object {

    private Position mPosition;

    public Graphics() {
        mPosition = new Position();
    }

    public Graphics(int xPos, int yPos) {
        mPosition = new Position(xPos, yPos);
    }

    public Graphics(Position position) {
        mPosition = position;
    }

    public Position getPosition() {
        return mPosition;
    }

    public void setPosition(Position value) {
        mPosition = value;
    }

    public void setPosition(int xpos, int ypos) {
        mPosition.setXPos(xpos);
        mPosition.setYPos(ypos);
    }

    public String getAbstractSyntax() {
        return "Graphics(" + (mPosition != null ? mPosition.getAbstractSyntax() : "") + ")";
    }

    public String getConcreteSyntax() {
        return (mPosition != null ? mPosition.getConcreteSyntax() : "");
    }

    public String getFormattedSyntax() {
        return "";
    }

    public Graphics getCopy() {
        return new Graphics(mPosition.getCopy());
    }

    public void writeXML(IndentWriter out) {
        out.println("<Graphics>").push();
        mPosition.writeXML(out);
        out.pop().println("</Graphics>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Position", new XMLParseAction() {

            public void run(Element element) {
                mPosition.parseXML(element);
            }
        });
    }
}
