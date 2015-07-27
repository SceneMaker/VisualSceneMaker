package de.dfki.vsm.model.sceneflow.graphics.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * The graphics information for an edge.
 *
 * @author Not me
 */
public class Graphics extends Syntax {
    private Arrow mArrow;

    public Graphics() {
        mArrow = new Arrow();
    }

    public Graphics(Arrow arrow) {
        mArrow = arrow;
    }

    public void setArrow(Arrow value) {
        mArrow = value;
    }

    public Arrow getArrow() {
        return mArrow;
    }

    public String getAbstractSyntax() {
        return "Graphics(" + ((mArrow != null)
                              ? mArrow.getAbstractSyntax()
                              : "") + ")";
    }

    public String getConcreteSyntax() {
        return ((mArrow != null)
                ? mArrow.getConcreteSyntax()
                : "");
    }

    public String getFormattedSyntax() {
        return "";
    }

    public Graphics getCopy() {
        return new Graphics(mArrow.getCopy());
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Graphics>").push();
        mArrow.writeXML(out);
        out.pop().println("</Graphics>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Arrow", new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mArrow.parseXML(element);
            }
        });
    }

    public int getHashCode() {
        int hashCode = 0;

        for (Point a : mArrow.getPointList()) {
            hashCode += a.getCtrlXPos();
            hashCode += a.getCtrlYPos();
        }

        return hashCode;
    }
}
