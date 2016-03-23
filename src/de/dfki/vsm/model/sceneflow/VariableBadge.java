package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.graphics.node.NodePosition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 */
public class VariableBadge implements ModelObject {
    protected NodePosition mPosition;
    protected String   mType;

    public VariableBadge(String type) {
        mPosition = new NodePosition(10, 10);
        mType     = type;
    }

    public VariableBadge(String type, NodePosition position) {
        mPosition = position;
    }

    public NodePosition getPosition() {
        return mPosition;
    }

    public void setPosition(NodePosition pos) {
        mPosition = pos;
    }

   
    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

                if (tag.equals("Position")) {
                    mPosition = new NodePosition();
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

    public SyntaxObject getCopy() {
        return null;
    }
}
