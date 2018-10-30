
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.util.EdgeGraphics;

import math.geom2d.Point2D;
import math.geom2d.line.Line2D;
import math.geom2d.spline.CubicBezierCurve2D;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Rectangle;

import java.util.ArrayList;
import java.util.Collection;

import static java.lang.Math.pow;

/**
 *
 * @author Souza Putra
 */
public class GridRectangle extends Rectangle {
    public static final int NODE_INTERSECTION = 0;
    public static final int EDGE_INTERSECTION = 1;
    public static final int NO_INTERSECTION   = -1;
    private boolean         aStarPath         = false;
    private final int       INITIAL_WEIGHT    = 1;
    private int             intersectionType  = -1;
    private int             rowIndex;
    private int             columnIndex;
    private int             weight;

    public GridRectangle() {
        super();
        weight = INITIAL_WEIGHT;
    }

    public GridRectangle(int x, int y, int width, int height) {
        super(x, y, width, height);
        this.weight = INITIAL_WEIGHT;
    }

    public void setRowIndex(int rowIndex) {
        this.rowIndex = rowIndex;
    }

    public void setColumnIndex(int columnIndex) {
        this.columnIndex = columnIndex;
    }

    public void setWeight(int weight) {
        this.weight = weight;

        // System.out.println("Setting weight: " + weight + " to " + columnIndex + "," + rowIndex);
    }

    public int getRowIndex() {
        return rowIndex;
    }

    public int getColumnIndex() {
        return columnIndex;
    }

    public int getWeight() {
        return weight;
    }

    public void setIndex(int row, int column) {
        this.rowIndex    = row;
        this.columnIndex = column;
    }

    public int getIntersectionType() {
        return intersectionType;
    }

    public void setIntersectionType(int intersectionType) {
        this.intersectionType = intersectionType;
    }

    public boolean isaStarPath() {
        return aStarPath;
    }

    public void setaStarPath(boolean aStarPath) {
        this.aStarPath = aStarPath;
    }

    public boolean isIntersectedbyNode(Node node) {
        double gridMinX = getX();
        double gridMaxX = getX() + getWidth();
        double gridMinY = getY();
        double gridMaxY = getY() + getHeight();
        double nodeMinX = node.getX();
        double nodeMaxX = node.getX() + node.getWidth();
        double nodeMinY = node.getY();
        double nodeMaxY = node.getY() + node.getHeight();

        return (gridMaxX >= nodeMinX) && (gridMinX <= nodeMaxX) && (gridMaxY >= nodeMinY) && (gridMinY <= nodeMaxY);
    }

    public boolean isIntersectByRectangle(EdgeGraphics edge) {
        CubicBezierCurve2D edgeBezier = new CubicBezierCurve2D(edge.mAbsoluteStartPos.getX(),
                                            edge.mAbsoluteStartPos.getY(), edge.mCCrtl1.getX(), edge.mCCrtl1.getY(),
                                            edge.mCCrtl2.getX(), edge.mCCrtl2.getY(), edge.mAbsoluteEndPos.getX(),
                                            edge.mAbsoluteEndPos.getY());
        double x1 = getX() - 2;
        double x2 = getX() + getWidth() + 2;
        double y1 = getY() - 2;
        double y2 = getY() + getHeight() + 2;

        // Get point of intersections on upper rectangle
        Line2D              upperRect         = new Line2D(x1, y1, x2, y1);
        Collection<Point2D> upperIntersection = edgeBezier.intersections(upperRect);

        if (!upperIntersection.isEmpty()) {

            // System.out.println("Found intersection on: " + getRowIndex() + "," + getColumnIndex());
            return true;
        }

        // Get point of intersections on left rectangle
        Line2D              leftRect         = new Line2D(x1, y1, x1, y2);
        Collection<Point2D> leftIntersection = edgeBezier.intersections(leftRect);

        if (!leftIntersection.isEmpty()) {

            // System.out.println("Found intersection on: " + getRowIndex() + "," + getColumnIndex());
            return true;
        }

        // Get point of intersections on right rectangle
        Line2D              rightRect         = new Line2D(x2, y1, x2, y2);
        Collection<Point2D> rightIntersection = edgeBezier.intersections(rightRect);

        if (!rightIntersection.isEmpty()) {

            // System.out.println("Found intersection on: " + getRowIndex() + "," + getColumnIndex());
            return true;
        }

        // Get point of intersections on right rectangle
        Line2D              bottomRect         = new Line2D(x1, y2, x2, y2);
        Collection<Point2D> bottomIntersection = edgeBezier.intersections(bottomRect);

        if (!bottomIntersection.isEmpty()) {

            // System.out.println("Found intersection on: " + getRowIndex() + "," + getColumnIndex());
            return true;
        }

        return false;
    }
}
