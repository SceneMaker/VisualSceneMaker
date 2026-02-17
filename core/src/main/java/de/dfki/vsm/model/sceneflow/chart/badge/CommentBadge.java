package de.dfki.vsm.model.sceneflow.chart.badge;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.glue.SyntaxObject;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentGraphics;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class CommentBadge implements ModelObject {

    protected SuperNode mParentNode = null;
    protected String mHTMLText = "";
    protected CommentGraphics mGraphics;

    public void setParentNode(SuperNode value) {
        mParentNode = value;
    }

    public CommentGraphics getGraphics() {
        return mGraphics;
    }

    public void setGraphics(CommentGraphics value) {
        mGraphics = value;
    }

    public String getHTMLText() {
        return mHTMLText.trim();
    }

    public void setHTMLText(String text) {
        mHTMLText = text.trim();
    }

    private void formatHTML() {
        if (mHTMLText == null) {
            mHTMLText = "";
        }
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

                if (tag.equals("Graphics")) {
                    mGraphics = new CommentGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("Text")) {
                    mHTMLText = element.getTextContent();
                } else {
                    throw new XMLParseError(null,
                            "Cannot parse the element with the tag \"" + tag
                            + "\" into a comment child!");
                }
            }
        });
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Comment>").push();

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        out.println("<Text style=\"color:blue\">").push();
        formatHTML();
        out.println(mHTMLText.trim());
        out.pop().println("</Text>");
        out.pop().println("</Comment>");
    }

    public SyntaxObject getCopy() {
        return null;
    }
}
