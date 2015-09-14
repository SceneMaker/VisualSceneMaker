
/*
* SceneflowEditor - GridManager
 */
package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.util.grid.GridConstants;
import de.dfki.vsm.editor.util.grid.GridRectangle;
import de.dfki.vsm.model.project.EditorConfig;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;

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
    private HashSet<Point> mPlacedNodes = new HashSet<>();
    private int            mNodesinRow  = 0;

    // Subgrid for A* algorithm
    private GridRectangle[][]        mTransitionArea      = null;
    private GridRectangle[][]        mTempTransitions     = null;
    private boolean                  isSubgridEstablished = false;
    private int                      height               = 0;
    private int                      width                = 0;
    private final ArrayList<Point2D> dockingPoints        = new ArrayList<>();
    private final WorkSpacePanel     mWorkSpacePanel;
    private final EditorConfig       mEditorConfig;
    private ArrayList<Rectangle>     mNodeAreas;
    private boolean                  isDebug;
    private boolean                  isDockingView;

    public GridManager(WorkSpacePanel ws) {
        mWorkSpacePanel    = ws;
        mEditorConfig = mWorkSpacePanel.getEditorConfig();
        isDebug       = mEditorConfig.sSHOW_SMART_PATH_DEBUG;
        isDockingView = false;
        compute();
    }

    public final void compute() {
        Dimension area = obtainWorkAreaSize();
        int       w    = area.width;
        int       h    = area.height;    // <-

        mNodesinRow = w / mEditorConfig.sGRID_XSPACE;
        mNodeAreas  = new ArrayList<>();

        if ((w / mEditorConfig.sGRID_XSPACE) > 0 && (h / mEditorConfig.sGRID_YSPACE) > 0
                && (isSubgridEstablished == false)) {
            mTransitionArea =
                new GridRectangle[((w / mEditorConfig.sGRID_XSPACE) + 1) * 2][((h / mEditorConfig.sGRID_YSPACE) + 1) * 2];
        }

        if (!((height == h / mEditorConfig.sGRID_YSPACE) && (width == w / mEditorConfig.sGRID_XSPACE))) {
            mTempTransitions =
                new GridRectangle[((w / mEditorConfig.sGRID_XSPACE) + 1) * 2][((h / mEditorConfig.sGRID_YSPACE) + 1) * 2];
        }

        int halfNodeSize = mEditorConfig.sGRID_NODEWIDTH / 2;

        for (int j = 0; j <= (h / mEditorConfig.sGRID_YSPACE); j++) {
            for (int i = 0; i <= (w / mEditorConfig.sGRID_XSPACE); i++) {
                Rectangle r = new Rectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE),
                                            mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE),
                                            mEditorConfig.sGRID_NODEWIDTH, mEditorConfig.sGRID_NODEWIDTH);

                mNodeAreas.add(r);

                // Initiates subgrids.
                if ((w / mEditorConfig.sGRID_XSPACE) > 0 && (h / mEditorConfig.sGRID_YSPACE) > 0
                        && (isSubgridEstablished == false)) {
                    GridRectangle s = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE) + 2,
                                          mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE) + 2,
                                          halfNodeSize - 4, halfNodeSize - 4);

                    s.setColumnIndex(j * 2);
                    s.setRowIndex(i * 2);
                    mTransitionArea[i * 2][j * 2] = s;

                    // System.out.println("(" + (i*2) + "," + (j*2) + ")");
                    GridRectangle t = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE)
                                          + halfNodeSize + 2, mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE)
                                              + 2, halfNodeSize - 4, halfNodeSize - 4);

                    t.setColumnIndex(j * 2);
                    t.setRowIndex(i * 2 + 1);
                    mTransitionArea[i * 2 + 1][j * 2] = t;

                    // System.out.println("(" + (i*2+1) + "," + (j*2) + ")");
                    GridRectangle u = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE) + 2,
                                          mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE) + halfNodeSize + 2,
                                          halfNodeSize - 4, halfNodeSize - 4);

                    u.setColumnIndex(j * 2 + 1);
                    u.setRowIndex(i * 2);
                    mTransitionArea[i * 2][j * 2 + 1] = u;

                    // System.out.println("(" + (i*2) + "," + (j*2+1) + ")");
                    GridRectangle v = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE)
                                          + halfNodeSize + 2, mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE)
                                              + halfNodeSize + 2, halfNodeSize - 4, halfNodeSize - 4);

                    mTransitionArea[i * 2 + 1][j * 2 + 1] = v;
                    v.setColumnIndex(j * 2 + 1);
                    v.setRowIndex(i * 2 + 1);

                    // System.out.println("(" + (i*2+1) + "," + (j*2+1) + ")");
                }

                if (!((height == (h / mEditorConfig.sGRID_YSPACE)) && (width == (w / mEditorConfig.sGRID_XSPACE)))) {
                    if ((j < height) && (i < width)) {
                        mTempTransitions[i * 2][j * 2] = mTransitionArea[i * 2][j * 2];
                        mTempTransitions[i * 2][j * 2].setaStarPath(mTransitionArea[i * 2][j * 2].isaStarPath());
                        mTempTransitions[i * 2][j * 2].setLocation(mEditorConfig.sXOFFSET
                                + (i * mEditorConfig.sGRID_XSPACE) + 2, mEditorConfig.sYOFFSET
                                    + (j * mEditorConfig.sGRID_YSPACE) + 2);
                        mTempTransitions[i * 2][j * 2].setSize(halfNodeSize - 4, halfNodeSize - 4);
                        mTempTransitions[i * 2 + 1][j * 2] = mTransitionArea[i * 2 + 1][j * 2];
                        mTempTransitions[i * 2 + 1][j * 2].setaStarPath(
                            mTransitionArea[i * 2 + 1][j * 2].isaStarPath());
                        mTempTransitions[i * 2 + 1][j * 2].setLocation(mEditorConfig.sXOFFSET
                                + (i * mEditorConfig.sGRID_XSPACE) + halfNodeSize + 2, mEditorConfig.sYOFFSET
                                    + (j * mEditorConfig.sGRID_YSPACE) + 2);
                        mTempTransitions[i * 2 + 1][j * 2].setSize(halfNodeSize - 4, halfNodeSize - 4);
                        mTempTransitions[i * 2][j * 2 + 1] = mTransitionArea[i * 2][j * 2 + 1];
                        mTempTransitions[i * 2][j * 2 + 1].setaStarPath(
                            mTransitionArea[i * 2][j * 2 + 1].isaStarPath());
                        mTempTransitions[i * 2][j * 2 + 1].setLocation(mEditorConfig.sXOFFSET
                                + (i * mEditorConfig.sGRID_XSPACE) + 2, mEditorConfig.sYOFFSET
                                    + (j * mEditorConfig.sGRID_YSPACE) + halfNodeSize + 2);
                        mTempTransitions[i * 2][j * 2 + 1].setSize(halfNodeSize - 4, halfNodeSize - 4);
                        mTempTransitions[i * 2 + 1][j * 2 + 1] = mTransitionArea[i * 2 + 1][j * 2 + 1];
                        mTempTransitions[i * 2 + 1][j * 2 + 1].setaStarPath(
                            mTransitionArea[i * 2 + 1][j * 2 + 1].isaStarPath());
                        mTempTransitions[i * 2 + 1][j * 2 + 1].setLocation(mEditorConfig.sXOFFSET
                                + (i * mEditorConfig.sGRID_XSPACE) + halfNodeSize + 2, mEditorConfig.sYOFFSET
                                    + (j * mEditorConfig.sGRID_YSPACE) + halfNodeSize + 2);
                        mTempTransitions[i * 2 + 1][j * 2 + 1].setSize(halfNodeSize - 4, halfNodeSize - 4);
                    } else {
                        GridRectangle s = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE)
                                              + 2, mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE) + 2,
                                                   halfNodeSize - 4, halfNodeSize - 4);

                        s.setColumnIndex(j * 2);
                        s.setRowIndex(i * 2);
                        mTempTransitions[i * 2][j * 2] = s;

                        // System.out.println("(" + (i*2) + "," + (j*2) + ")");
                        GridRectangle t = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE)
                                              + halfNodeSize + 2, mEditorConfig.sYOFFSET
                                                  + (j * mEditorConfig.sGRID_YSPACE) + 2, halfNodeSize - 4, halfNodeSize
                                                      - 4);

                        t.setColumnIndex(j * 2);
                        t.setRowIndex(i * 2 + 1);
                        mTempTransitions[i * 2 + 1][j * 2] = t;

                        // System.out.println("(" + (i*2+1) + "," + (j*2) + ")");
                        GridRectangle u = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE)
                                              + 2, mEditorConfig.sYOFFSET + (j * mEditorConfig.sGRID_YSPACE)
                                                   + halfNodeSize + 2, halfNodeSize - 4, halfNodeSize - 4);

                        u.setColumnIndex(j * 2 + 1);
                        u.setRowIndex(i * 2);
                        mTempTransitions[i * 2][j * 2 + 1] = u;

                        // System.out.println("(" + (i*2) + "," + (j*2+1) + ")");
                        GridRectangle v = new GridRectangle(mEditorConfig.sXOFFSET + (i * mEditorConfig.sGRID_XSPACE)
                                              + halfNodeSize + 2, mEditorConfig.sYOFFSET
                                                  + (j * mEditorConfig.sGRID_YSPACE) + halfNodeSize + 2, halfNodeSize
                                                      - 4, halfNodeSize - 4);

                        mTempTransitions[i * 2 + 1][j * 2 + 1] = v;
                        v.setColumnIndex(j * 2 + 1);
                        v.setRowIndex(i * 2 + 1);
                    }
                }
            }
        }

        if ((w / mEditorConfig.sGRID_XSPACE) > 0 && (h / mEditorConfig.sGRID_YSPACE) > 0
                && (isSubgridEstablished == false)) {
            isSubgridEstablished = true;
            height               = h / mEditorConfig.sGRID_YSPACE;
            width                = w / mEditorConfig.sGRID_XSPACE;
        }

        if (!((height == h / mEditorConfig.sGRID_YSPACE) && (width == w / mEditorConfig.sGRID_XSPACE))) {
            mTransitionArea = mTempTransitions;
            height          = h / mEditorConfig.sGRID_YSPACE;
            width           = w / mEditorConfig.sGRID_XSPACE;
        }
    }

    public void update() {
        isDebug      = mEditorConfig.sSHOW_SMART_PATH_DEBUG;
        mPlacedNodes = new HashSet<>();
        compute();
    }

    private Dimension obtainWorkAreaSize() {
        int w = 0;
        int h = 0;

        for (de.dfki.vsm.model.sceneflow.Node n : mWorkSpacePanel.getSceneFlowEditor().getSceneFlow().getNodeList()) {
            if (n.getGraphics().getPosition().getXPos() > w) {
                w = n.getGraphics().getPosition().getXPos() + mEditorConfig.sNODEWIDTH;
            }

            if (n.getGraphics().getPosition().getYPos() > h) {
                h = n.getGraphics().getPosition().getYPos() + mEditorConfig.sNODEHEIGHT;
            }
        }

        for (de.dfki.vsm.model.sceneflow.SuperNode n :
                mWorkSpacePanel.getSceneFlowEditor().getSceneFlow().getSuperNodeList()) {
            if (n.getGraphics().getPosition().getXPos() > w) {
                w = n.getGraphics().getPosition().getXPos() + mEditorConfig.sNODEWIDTH;
            }

            if (n.getGraphics().getPosition().getYPos() > h) {
                h = n.getGraphics().getPosition().getYPos() + mEditorConfig.sNODEHEIGHT;
            }
        }
            
        if(mWorkSpacePanel.getSize().height>h){
            h = mWorkSpacePanel.getSize().height;
        }

        if(mWorkSpacePanel.getSize().width>w){
            w = mWorkSpacePanel.getSize().width;
        }
    

        return new Dimension(w, h);
    }

    public void drawGrid(Graphics2D g2d) {
        compute();

        if (mEditorConfig.sSHOWGRID) {
            g2d.setStroke(new BasicStroke(1.0f));

            for (Rectangle r : mNodeAreas) {
                int ai = mNodeAreas.indexOf(r);

                // draw a litte cross
                g2d.setColor(Color.GRAY.brighter());

                // g2d.setColor(new Color(230, 230, 230, 200).darker());
                g2d.drawLine(r.x + r.width / 2 - 2, r.y + r.height / 2, r.x + r.width / 2 + 2, r.y + r.height / 2);
                g2d.drawLine(r.x + r.width / 2, r.y + r.height / 2 - 2, r.x + r.width / 2, r.y + r.height / 2 + 2);

                // draw node areas
                // g2d.drawRect(r.x, r.y, r.width, r.height);
                // g2d.drawString("" + ai, r.x + 2, r.y + 12);
            }

            if (isDebug) {
                for (GridRectangle[] r : mTransitionArea) {
                    for (GridRectangle s : r) {
                        int ai = mNodeAreas.indexOf(r);

                        // draw a litte cross
                        g2d.setColor(new Color(230, 230, 230, 200));
                        g2d.drawLine(s.x + s.width / 2 - 2, s.y + s.height / 2, s.x + s.width / 2 + 2,
                                     s.y + s.height / 2);
                        g2d.drawLine(s.x + s.width / 2, s.y + s.height / 2 - 2, s.x + s.width / 2,
                                     s.y + s.height / 2 + 2);

                        // draw node areas
                        if (s.getWeight() > 1) {
                            g2d.setColor(Color.red);
                            g2d.drawString("" + s.getWeight(), s.x + 2, s.y + s.height / 2 + 6);
                        }

                        if (s.isaStarPath()) {
                            g2d.setColor(Color.blue);
                        }

                        g2d.drawRect(s.x, s.y, s.width, s.height);
                        g2d.drawString("" + s.getColumnIndex() + "," + s.getRowIndex(), s.x + 2, s.y + 12);
                    }
                }
            }
        }

        if (isDockingView) {
            for (int i = 0; i < this.dockingPoints.size(); i++) {
                g2d.setColor(new Color(0, 0, 255, 255));
                g2d.setBackground(new Color(0, 0, 255, 255));
                g2d.drawOval((int) Math.round(this.dockingPoints.get(i).getX() - 10),
                             (int) Math.round(this.dockingPoints.get(i).getY() - 10), 20, 20);
            }
        }
    }

    public void setDebugMode(boolean status) {
        this.isDebug = status;
        update();
    }

    public void setDockingView(boolean status) {
        this.isDockingView = status;
        update();
    }

    public void addDockingPoints(Point2D point) {
        this.dockingPoints.add(point);
    }

    public void deleteDockingPoints(Point2D point) {
        for (int i = 0; i < this.dockingPoints.size(); i++) {
            if ((this.dockingPoints.get(i).getX() == point.getX())
                    && (this.dockingPoints.get(i).getY() == point.getY())) {
                this.dockingPoints.remove(i);

                break;
            }
        }
    }

    public Point getNodeLocation(Point inputPoint) {
        Point p = new Point(inputPoint.x + mEditorConfig.sGRID_NODEWIDTH / 2,
                            inputPoint.y + mEditorConfig.sGRID_NODEWIDTH / 2);

        for (Rectangle r : mNodeAreas) {
            if (r.contains(p)) {
                p = new Point(r.x, r.y);

                break;
            }
        }

        // check if p is already in set of grid points
        if (mPlacedNodes.contains(p)) {

            // System.out.println("point already in use!");
            p = findNextFreePosition(p);
        }

        mPlacedNodes.add(p);

        return p;
    }

    public void freeGridPosition(Point p) {
        if (mPlacedNodes.contains(p)) {

            // System.out.println("point is in use - delete in occupied positions");
            mPlacedNodes.remove(p);
        }
    }

    public GridRectangle[][] getmTransitionArea() {
        return mTransitionArea;
    }

    public void setNodeWeight(Node node) {
        for (GridRectangle[] gridParent : mTransitionArea) {
            for (GridRectangle gridRectangle : gridParent) {
                if (gridRectangle.isIntersectedbyNode(node)) {
                    gridRectangle.setWeight(GridConstants.NODE_WEIGHT);

//                  System.out.println("Setting weight of " + 
//                          GridConstants.NODE_WEIGHT + " to Grid <" +
//                          gridRectangle.getColumnIndex() + "," +
//                          gridRectangle.getRowIndex() + ">");
                }
            }
        }
    }

    public void setEdgeWeight(Edge edge) {
        for (GridRectangle[] gridParent : mTransitionArea) {
            for (GridRectangle gridRectangle : gridParent) {
                if (gridRectangle.isIntersectByRectangle(edge.mEg)) {
                    gridRectangle.setWeight(GridConstants.EDGE_WEIGHT);

//                  System.out.println("Setting weight of " + 
//                          GridConstants.EDGE_WEIGHT + " to Grid <" +
//                          gridRectangle.getColumnIndex() + "," +
//                          gridRectangle.getRowIndex() + ">");
                }
            }
        }
    }

    public void resetGridWeight(Edge edge) {
        for (GridRectangle[] gridParent : mTransitionArea) {
            for (GridRectangle gridRectangle : gridParent) {
                if (gridRectangle.isIntersectByRectangle(edge.mEg)) {
                    gridRectangle.setWeight(GridConstants.INITIAL_WEIGHT);

//                  System.out.println("Setting weight of " + 
//                          GridConstants.INITIAL_WEIGHT + " to Grid <" +
//                          gridRectangle.getColumnIndex() + "," +
//                          gridRectangle.getRowIndex() + ">");
                }
            }
        }
    }

    public void resetGridWeight(Node node) {
        for (GridRectangle[] gridParent : mTransitionArea) {
            for (GridRectangle gridRectangle : gridParent) {
                if (gridRectangle.isIntersectedbyNode(node)) {
                    gridRectangle.setWeight(GridConstants.INITIAL_WEIGHT);

//                  System.out.println("Setting weight of " + 
//                          GridConstants.INITIAL_WEIGHT + " to Grid <" +
//                          gridRectangle.getColumnIndex() + "," +
//                          gridRectangle.getRowIndex() + ">");
                }
            }
        }
    }

    public void normalizeGridWeight() {
        for (GridRectangle[] gridParent : mTransitionArea) {
            for (GridRectangle gridRectangle : gridParent) {
                boolean isGridInteresected = false;

                for (Edge edge : mWorkSpacePanel.getEdges()) {
                    if (gridRectangle.isIntersectByRectangle(edge.mEg)) {
                        gridRectangle.setWeight(GridConstants.EDGE_WEIGHT);
                        isGridInteresected = true;
                    }
                }

                for (Node node : mWorkSpacePanel.getNodes()) {
                    if (gridRectangle.isIntersectedbyNode(node)) {
                        gridRectangle.setWeight(GridConstants.NODE_WEIGHT);
                        isGridInteresected = true;
                    }
                }

                if (isGridInteresected == false) {
                    gridRectangle.setWeight(GridConstants.INITIAL_WEIGHT);
                }

                if (gridRectangle.isaStarPath()) {
                    gridRectangle.setaStarPath(false);
                }
            }
        }
    }

    public void resetAllGridWeight() {
        for (GridRectangle[] gridParent : mTransitionArea) {
            for (GridRectangle gridRectangle : gridParent) {
                gridRectangle.setWeight(GridConstants.INITIAL_WEIGHT);
            }
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
        int x    = 0,
            y    = 0,
            dx   = 0,
            dy   = -1;
        int t    = Math.max(mNodesinRow, mNodesinRow);
        int maxI = t * t;

        for (int i = 0; i < maxI; i++) {
            if ((-mNodesinRow / 2 <= x) && (x <= mNodesinRow / 2) && (-mNodesinRow / 2 <= y)
                    && (y <= mNodesinRow / 2)) {
                if (i > 0) {
                    if ((iPoint.x - (x * mEditorConfig.sGRID_XSPACE) > 0)
                            && (iPoint.y - (y * mEditorConfig.sGRID_YSPACE) > 0)) {    // check if position is not outside the workspace on the left / top
                        Point p = new Point(iPoint.x - (x * mEditorConfig.sGRID_XSPACE),
                                            iPoint.y - (y * mEditorConfig.sGRID_YSPACE));

                        if (!mPlacedNodes.contains(p)) {
                            return p;
                        }
                    }
                }
            }

            if ((x == y) || ((x < 0) && (x == -y)) || ((x > 0) && (x == 1 - y))) {
                t  = dx;
                dx = -dy;
                dy = t;
            }

            x += dx;
            y += dy;
        }

        return null;
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
}
