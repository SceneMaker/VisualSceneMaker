package de.dfki.vsm.model.sceneflow.diagram.graphics.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
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
public class EdgeArrow implements ModelObject {
    private Vector<EdgePoint> mPointList;

    public EdgeArrow() {
        mPointList = new Vector<EdgePoint>();
    }

    public EdgeArrow(Vector<EdgePoint> pointList) {
        mPointList = pointList;
    }

    public void setPointList(Vector<EdgePoint> value) {
        mPointList = value;
    }

    public Vector<EdgePoint> getPointList() {
        return mPointList;
    }

    public int getSizeOfPointList() {
        return mPointList.size();
    }

    public Vector<EdgePoint> getCopyOfPointList() {
        Vector<EdgePoint> copy = new Vector<EdgePoint>();

        for (EdgePoint point : mPointList) {
            copy.add(point.getCopy());
        }

        return copy;
    }

//    public String getAbstractSyntax() {
//        String desc = "Arrow(";
//
//        for (int i = 0; i < mPointList.size(); i++) {
//            desc += mPointList.get(i).getAbstractSyntax();
//
//            if (i != mPointList.size() - 1) {
//                desc += ",";
//            }
//        }
//
//        return desc + ")";
//    }
//
//    public String getConcreteSyntax() {
//        String desc = "";
//
//        for (int i = 0; i < mPointList.size(); i++) {
//            desc += mPointList.get(i).getConcreteSyntax();
//
//            if (i != mPointList.size() - 1) {
//                desc += ",";
//            }
//        }
//
//        return desc;
//    }
//
//    public String getFormattedSyntax() {
//        return "";
//    }

    @Override
    public EdgeArrow getCopy() {
        return new EdgeArrow(getCopyOfPointList());
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Arrow>").push();

        for (int i = 0; i < mPointList.size(); i++) {
            mPointList.get(i).writeXML(out);
        }

        out.pop().println("</Arrow>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Point", new XMLParseAction() {
            @Override
            public void run(Element element) {
                EdgePoint point = new EdgePoint();

                point.parseXML(element);
                mPointList.add(point);
            }
        });
    }
}
