package de.dfki.vsm.model.sceneflow.graphics.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import java.util.ArrayList;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------


/**
 * An arrow of an edge.
 *
 * @author Not me
 */
public class Arrow extends Syntax {
    private ArrayList<Point> mPointList;

    public Arrow() {
        mPointList = new ArrayList<Point>();
    }

    public Arrow(ArrayList<Point> pointList) {
        mPointList = pointList;
    }

    public void setPointList(ArrayList<Point> value) {
        mPointList = value;
    }

    public ArrayList<Point> getPointList() {
        return mPointList;
    }

    public int getSizeOfPointList() {
        return mPointList.size();
    }

    public ArrayList<Point> getCopyOfPointList() {
        ArrayList<Point> copy = new ArrayList<Point>();

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
