package de.dfki.vsm.model.sceneflow.diagram.graphics.node;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class NodeGraphics implements ModelObject {
    private NodePosition mPosition;

    public NodeGraphics() {
        mPosition = new NodePosition();
    }

    public NodeGraphics(NodePosition position) {
        mPosition = position;
    }

    public NodeGraphics(int xPos, int yPos) {
        mPosition = new NodePosition(xPos, yPos);
    }

    public NodePosition getPosition() {
        return mPosition;
    }

    public void setPosition(NodePosition value) {
        mPosition = value;
    }

    public void setPosition(int xpos, int ypos) {
        mPosition.setXPos(xpos);
        mPosition.setYPos(ypos);
    }

//    public String getAbstractSyntax() {
//        return "Graphics(" + ((mPosition != null)
//                              ? mPosition.getAbstractSyntax()
//                              : "") + ")";
//    }
//
//    public String getConcreteSyntax() {
//        return ((mPosition != null)
//                ? mPosition.getConcreteSyntax()
//                : "");
//    }
//
//    public String getFormattedSyntax() {
//        return "";
//    }

    @Override
    public NodeGraphics getCopy() {
        return new NodeGraphics(mPosition.getCopy());
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Graphics>").push();
        mPosition.writeXML(out);
        out.pop().println("</Graphics>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Position", new XMLParseAction() {
            @Override
            public void run(Element element) {
                mPosition.parseXML(element);
            }
        });
    }
}
