package de.dfki.vsm.model.sceneflow;

import de.dfki.vsm.model.sceneflow.graphics.node.Position;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public class VariableBadge extends Object {

    protected Position mPosition;

    public VariableBadge() {
        mPosition = new Position(10, 10);
    }

    public VariableBadge(Position position) {
        mPosition = position;
    }

    public Position getPosition() {
        return mPosition;
    }

    public void setPosition(Position pos) {
        mPosition = pos;
    }

    @Override
    public String getAbstractSyntax() {
        return "";
    }

    @Override
    public String getConcreteSyntax() {
        return "";
    }

    @Override
    public String getFormattedSyntax() {
        return "";
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {

            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();
                if (tag.equals("Position")) {
                    mPosition = new Position();
                    mPosition.parseXML(element);
                } else {
                    throw new XMLParseError(null,
                            "Cannot parse the element with the tag \"" + tag + "\" into a comment child!");
                }
            }
        });
    }

    public void writeXML(IndentWriter out) {
        out.println("<VariableBadge>").push();
        if (mPosition != null) {
            mPosition.writeXML(out);
        }
        out.pop().println("</VariableBadge>");
    }

    public Object getCopy() {
        return null;
    }
}
