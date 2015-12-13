package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.graphics.Position;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public class VariableBadge extends Syntax {
    protected Position mPosition;
    protected String   mType;

    public VariableBadge(String type) {
        mPosition = new Position(10, 10);
        mType     = type;
    }

    public VariableBadge(String type, Position position) {
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
                                            "Cannot parse the element with the tag \"" + tag
                                            + "\" into a comment child!");
                }
            }
        });
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<" + mType + ">").push();

        if (mPosition != null) {
            mPosition.writeXML(out);
        }

        out.pop().println("</" + mType + ">");
    }

    public Syntax getCopy() {
        return null;
    }
}
