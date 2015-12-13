package de.dfki.vsm.model.sceneflow.graphics.node;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.graphics.Position;
import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class Graphics extends Syntax {
    private Position mPosition;

    public Graphics() {
        mPosition = new Position();
    }

    public Graphics(Position position) {
        mPosition = position;
    }

    public Graphics(int xPos, int yPos) {
        mPosition = new Position(xPos, yPos);
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
        return "Graphics(" + ((mPosition != null)
                              ? mPosition.getAbstractSyntax()
                              : "") + ")";
    }

    public String getConcreteSyntax() {
        return ((mPosition != null)
                ? mPosition.getConcreteSyntax()
                : "");
    }

    public String getFormattedSyntax() {
        return "";
    }

    public Graphics getCopy() {
        return new Graphics(mPosition.getCopy());
    }

    public void writeXML(IOSIndentWriter out) {
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
