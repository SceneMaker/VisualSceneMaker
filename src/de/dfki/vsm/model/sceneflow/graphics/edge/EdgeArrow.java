package de.dfki.vsm.model.sceneflow.graphics.edge;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class EdgeArrow implements ModelObject {

    // The control point list
    private ArrayList<EdgePoint> mPointList;

    // Create the connection
    public EdgeArrow() {
        mPointList = new ArrayList();
    }

    // Create the connection
    public EdgeArrow(final ArrayList pointList) {
        mPointList = pointList;
    }

    // Set the point list
    public void setPointList(final ArrayList<EdgePoint> value) {
        mPointList = value;
    }

    // Get the point list
    public final ArrayList<EdgePoint> getPointList() {
        return mPointList;
    }

    // Copy the point list
    public final ArrayList<EdgePoint> getCopyOfPointList() {
        final ArrayList<EdgePoint> copy = new ArrayList();
        for (final EdgePoint point : mPointList) {
            copy.add(point.getCopy());
        }
        return copy;
    }

    // Copy the connection
    @Override
    public final EdgeArrow getCopy() {
        return new EdgeArrow(getCopyOfPointList());
    }

    // Write the connection
    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<Connection>").push();
        for (int i = 0; i < mPointList.size(); i++) {
            mPointList.get(i).writeXML(out);
        }
        out.pop().println("</Connection>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "ControlPoint", new XMLParseAction() {
            @Override
            public void run(final Element element) {
                final EdgePoint point = new EdgePoint();
                point.parseXML(element);
                mPointList.add(point);
            }
        });
    }
}