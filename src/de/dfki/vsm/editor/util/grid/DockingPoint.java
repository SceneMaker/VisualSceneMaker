
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util.grid;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.Node.Type;

//~--- JDK imports ------------------------------------------------------------

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 *
 * @author Souza Putra
 */
public class DockingPoint {
    private final Rectangle2D rectangle;
    private final Line2D      vectorLine;
    private final Point2D     intersectionLine;

    public DockingPoint(Node node, Point2D controlPoint) {
        this.rectangle  = new Rectangle2D.Double(node.getX(), node.getY(), node.getWidth(), node.getHeight());
        this.vectorLine = new Line2D.Double(this.rectangle.getCenterX(), this.rectangle.getCenterY(),
                controlPoint.getX(), controlPoint.getY());
        this.intersectionLine = getIntersectionPoint(vectorLine, rectangle, node);
    }

    public int getIntersectionX() {
        if (intersectionLine != null) {

            // System.out.println("X Intersection: " + (int) Math.round(this.intersectionLine[0].getX()));
            return (int) Math.round(this.intersectionLine.getX());
        }

        return -1;
    }

    public int getIntersectionY() {
        if (intersectionLine != null) {

            // System.out.println("Y Intersection: " + (int) Math.round(this.intersectionLine[0].getY()));
            return (int) Math.round(this.intersectionLine.getY());
        }

        return -1;
    }

    public Point2D getIntersectionPoint(Line2D line, Rectangle2D rectangle, Node node) {
        if (node.getType() == Type.SuperNode) {

            // System.out.println("This is a super node!");
            Line2D topLine = new Line2D.Double(rectangle.getX(), rectangle.getY(),
                                               rectangle.getX() + rectangle.getWidth(), rectangle.getY());
            Line2D bottomLine = new Line2D.Double(rectangle.getX(), rectangle.getY() + rectangle.getHeight(),
                                    rectangle.getX() + rectangle.getWidth(), rectangle.getY() + rectangle.getHeight());
            Line2D leftLine = new Line2D.Double(rectangle.getX(), rectangle.getY(), rectangle.getX(),
                                  rectangle.getY() + rectangle.getHeight());
            Line2D rightLine = new Line2D.Double(rectangle.getX() + rectangle.getWidth(), rectangle.getY(),
                                   rectangle.getX() + rectangle.getWidth(), rectangle.getY() + rectangle.getHeight());

            // Top line
            if (intersects(line, topLine)) {
                return getIntersectionPoint(line, topLine);
            }

            // Bottom line
            if (intersects(line, bottomLine)) {
                return getIntersectionPoint(line, bottomLine);
            }

            // Left side...
            if (intersects(line, leftLine)) {
                return getIntersectionPoint(line, leftLine);
            }

            // Right side
            if (intersects(line, rightLine)) {
                return getIntersectionPoint(line, rightLine);
            }
        } else if (node.getType() == Type.BasicNode) {

            // System.out.println("This is a basic node!");
            Line2D topLine = new Line2D.Double(rectangle.getCenterX(), rectangle.getY(), rectangle.getX(),
                                               rectangle.getCenterY());
            Line2D bottomLine = new Line2D.Double(rectangle.getX(), rectangle.getCenterY(), rectangle.getCenterX(),
                                    rectangle.getY() + rectangle.getHeight());
            Line2D leftLine = new Line2D.Double(rectangle.getCenterX(), rectangle.getY() + rectangle.getHeight(),
                                  rectangle.getX() + rectangle.getWidth(), rectangle.getCenterY());
            Line2D rightLine = new Line2D.Double(rectangle.getX() + rectangle.getWidth(), rectangle.getCenterY(),
                                   rectangle.getCenterX(), rectangle.getY());

            // Top line
            if (intersects(line, topLine)) {
                return getIntersectionPoint(line, topLine);
            }

            // Bottom line
            if (intersects(line, bottomLine)) {
                return getIntersectionPoint(line, bottomLine);
            }

            // Left side...
            if (intersects(line, leftLine)) {
                return getIntersectionPoint(line, leftLine);
            }

            // Right side
            if (intersects(line, rightLine)) {
                return getIntersectionPoint(line, rightLine);
            }

            // return getLineCircleIntersection(this.vectorLine, this.rectangle);
        }

        return null;
    }

    public Point2D getIntersectionPoint(Line2D lineA, Line2D lineB) {
        double  x1 = lineA.getX1();
        double  y1 = lineA.getY1();
        double  x2 = lineA.getX2();
        double  y2 = lineA.getY2();
        double  x3 = lineB.getX1();
        double  y3 = lineB.getY1();
        double  x4 = lineB.getX2();
        double  y4 = lineB.getY2();
        Point2D p  = null;
        double  d  = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);

        if (d != 0) {
            double xi = ((x3 - x4) * (x1 * y2 - y1 * x2) - (x1 - x2) * (x3 * y4 - y3 * x4)) / d;
            double yi = ((y3 - y4) * (x1 * y2 - y1 * x2) - (y1 - y2) * (x3 * y4 - y3 * x4)) / d;

            p = new Point2D.Double(xi, yi);
        }

        return p;
    }

    public boolean intersects(Line2D line1, Line2D line2) {
        return line1.intersectsLine(line2);
    }

    public Point2D getLineCircleIntersection(Line2D line, Rectangle2D rectangle) {
        double radius = Math.abs(rectangle.getCenterY() - rectangle.getY());
        double baX    = Math.abs(line.getX2() - line.getX1());
        double baY    = Math.abs(line.getY2() - line.getY1());
        double caX    = Math.abs(rectangle.getCenterX() - line.getX1());
        double caY    = Math.abs(rectangle.getCenterY() - line.getY1());
        double a      = baX * baX + baY * baY;
        double bBy2   = baX * caX + baY * caY;
        double c      = caX * caX + caY * caY - radius * radius;
        double pBy2   = bBy2 / a;
        double q      = c / a;
        double disc   = pBy2 * pBy2 - q;

        // if disc == 0 ... dealt with later
        double tmpSqrt          = Math.sqrt(disc);
        double abScalingFactor1 = -pBy2 + tmpSqrt;

        // System.out.println((line.getX1() - baX * abScalingFactor1) + "," +
        // (line.getY1() - baY * abScalingFactor1));
        return new Point2D.Double(line.getX1() - baX * abScalingFactor1, line.getY1() - baY * abScalingFactor1);
    }
}
