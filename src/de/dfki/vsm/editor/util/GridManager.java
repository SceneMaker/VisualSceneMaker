/*
 * SceneflowEditor - GridManager
 */
package de.dfki.vsm.editor.util;

import de.dfki.vsm.editor.WorkSpace;
import static de.dfki.vsm.editor.util.Preferences.sGRID_NODEWIDTH;
import static de.dfki.vsm.editor.util.Preferences.sGRID_XSPACE;
import static de.dfki.vsm.editor.util.Preferences.sGRID_YSPACE;
import static de.dfki.vsm.editor.util.Preferences.sSHOWGRID;
import static de.dfki.vsm.editor.util.Preferences.sXOFFSET;
import static de.dfki.vsm.editor.util.Preferences.sYOFFSET;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;

/*
 * @author Patrick
 * This class manages the node placement on the workspace.
 * Additional methods are provided for an intelligent placement 
 * of nodes
 */
public class GridManager {

  private HashSet<Point> mPlacedNodes = new HashSet<Point>();
  private WorkSpace mWorkSpace;
  private ArrayList<Rectangle> mNodeAreas;
  private int mNodesinRow = 0;

  public GridManager(WorkSpace ws) {
    mWorkSpace = ws;
    compute();
  }

  class PComparator implements Comparator<Point> {

    public int compare(Point p1, Point p2) {
      long p1xy = p1.x * p1.x + p1.y * p1.y;
      long p2xy = p2.x * p2.x + p2.y * p2.y;

      if (p1xy < p2xy) {
        return -1;
      } else {
        return 1;
      }
    }
  }

  private void compute() {
    Dimension size = mWorkSpace.getSize();
    Insets insets = mWorkSpace.getInsets();
    int w = size.width - insets.left - insets.right;
    int h = size.height - insets.top - insets.bottom;
    mNodesinRow = w / sGRID_XSPACE;
    mNodeAreas = new ArrayList<Rectangle>();
    int halfNodeSize = sGRID_NODEWIDTH / 2;
    for (int j = 0; j <= (h / sGRID_YSPACE); j++) {
      for (int i = 0; i <= (w / sGRID_XSPACE); i++) {
        Rectangle r = new Rectangle(sXOFFSET + (i * sGRID_XSPACE), sYOFFSET + (j * sGRID_YSPACE), sGRID_NODEWIDTH, sGRID_NODEWIDTH);
        mNodeAreas.add(r);
      }
    }
  }

  public void update() {
    mPlacedNodes = new HashSet<Point>();
  }

  public void drawGrid(Graphics2D g2d) {
    compute();

    if (sSHOWGRID) {
      g2d.setColor(Color.GRAY.brighter());
      g2d.setStroke(new BasicStroke(1.0f));

      for (Rectangle r : mNodeAreas) {
        int ai = mNodeAreas.indexOf(r);
        // draw a litte cross
        g2d.setColor(new Color(230, 230, 230, 200));
        g2d.drawLine(r.x + r.width / 2 - 2, r.y + r.height / 2, r.x + r.width / 2 + 2, r.y + r.height / 2);
        g2d.drawLine(r.x + r.width / 2, r.y + r.height / 2 - 2, r.x + r.width / 2, r.y + r.height / 2 + 2);
        // draw node areas
        // g2d.drawRect(r.x, r.y, r.width, r.height);
        // g2d.drawString("" + ai, r.x + 2, r.y + 12);
      }
    }
  }

  public Point getNodeLocation(Point inputPoint) {
    Point p = new Point(inputPoint.x + sGRID_NODEWIDTH / 2, inputPoint.y + sGRID_NODEWIDTH / 2);
    for (Rectangle r : mNodeAreas) {
      if (r.contains(p)) {
        p = new Point(r.x, r.y);
        break;
      }
    }

    // check if p is already in set of grid points
    if (mPlacedNodes.contains(p)) {
      //System.out.println("point already in use!");
      p = findNextFreePosition(p);
    }
    mPlacedNodes.add(p);
    return p;
  }

  public void freeGridPosition(Point p) {
    if (mPlacedNodes.contains(p)) {
      //System.out.println("point is in use - delete in occupied positions");
      mPlacedNodes.remove(p);
    }
  }

/*
 * This method spirals around an occupied grid point in order to find a free
 * grid position for a new or moved node. It starts looking for a free grid
 * position left to the occupied grid place, then proceeds clockwise in a spiral
 * around that place. 
 * Code used from: JHolta (http://stackoverflow.com/questions/398299/looping-in-a-spiral/10607084#10607084)
 */
  private Point findNextFreePosition(Point iPoint) {
    int x = 0, y = 0, dx = 0, dy = -1;
    int t = Math.max(mNodesinRow, mNodesinRow);
    int maxI = t * t;

    for (int i = 0; i < maxI; i++) {
      if ((-mNodesinRow / 2 <= x) && (x <= mNodesinRow / 2) && (-mNodesinRow / 2 <= y) && (y <= mNodesinRow / 2)) {
        if (i > 0) {
          if ((iPoint.x - (x * sGRID_XSPACE) > 0)
            && (iPoint.y - (y * sGRID_YSPACE) > 0)) { // check if position is not outside the workspace on the left / top

            Point p = new Point(iPoint.x - (x * sGRID_XSPACE), iPoint.y - (y * sGRID_YSPACE));

            if (!mPlacedNodes.contains(p)) {
              return p;
            }
          }
        }
      }

      if ((x == y) || ((x < 0) && (x == -y)) || ((x > 0) && (x == 1 - y))) {
        t = dx;
        dx = -dy;
        dy = t;
      }
      x += dx;
      y += dy;
    }

    return null;
  }
}
