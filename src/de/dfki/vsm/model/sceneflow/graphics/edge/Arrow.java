package de.dfki.vsm.model.sceneflow.graphics.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 * An arrow of an edge.
 *
 * @author Not me
 */
public class Arrow extends Syntax {
    private Vector<Point> mPointList;

    public Arrow() {
        mPointList = new Vector<Point>();
    }

    public Arrow(Vector<Point> pointList) {
        mPointList = pointList;
    }

    public void setPointList(Vector<Point> value) {
        mPointList = value;
    }

    public Vector<Point> getPointList() {
        return mPointList;
    }

    public int getSizeOfPointList() {
        return mPointList.size();
    }

    public Vector<Point> getCopyOfPointList() {
        Vector<Point> copy = new Vector<Point>();

        for (Point point : mPointList) {
            copy.add(point.getCopy());
        }

        return copy;
    }

    public String getAbstractSyntax() {
        String desc = "Arrow(";

        for (int i = 0; i < mPointList.size(); i++) {
            desc += mPointList.get(i).getAbstractSyntax();

            if (i != mPointList.size() - 1) {
                desc += ",";
            }
        }

        return desc + ")";
    }

    public String getConcreteSyntax() {
        String desc = "";

        for (int i = 0; i < mPointList.size(); i++) {
            desc += mPointList.get(i).getConcreteSyntax();

            if (i != mPointList.size() - 1) {
                desc += ",";
            }
        }

        return desc;
    }

    public String getFormattedSyntax() {
        return "";
    }

    public Arrow getCopy() {
        return new Arrow(getCopyOfPointList());
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Arrow>").push();

        for (int i = 0; i < mPointList.size(); i++) {
            mPointList.get(i).writeXML(out);
        }

        out.pop().println("</Arrow>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Point", new XMLParseAction() {
            public void run(Element element) {
                Point point = new Point();

                point.parseXML(element);
                mPointList.add(point);
            }
        });
    }
}
