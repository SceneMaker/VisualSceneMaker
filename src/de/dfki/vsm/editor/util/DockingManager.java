package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Point;

import java.util.Hashtable;
import java.util.Set;
import java.util.Vector;

/**
 * EdgeNodeDockingManager manages incoming and outgoing edges of a
 * node/supernode Initially nodes have 24 free dock positions where edges con be
 * connected
 *
 * @author Patrick Gebhard
 */
public class DockingManager {
    private Node                       mGUINode              = null;
    private Node.Type                  mNodeType             = null;    // The type defines the location of the dock points
    private Vector<DockPoint>          mDockPoints           = new Vector<DockPoint>();
    private Hashtable<Edge, DockPoint> mEdgeDockPoints       = new Hashtable<Edge, DockPoint>();
    private Hashtable<Edge, DockPoint> mEdgeSecondDockPoints = new Hashtable<Edge, DockPoint>();
    private final LOGDefaultLogger     mLogger               = LOGDefaultLogger.getInstance();
    private EditorConfig         mEditorConfig;

    public DockingManager(Node node) {
        mGUINode     = node;
        mEditorConfig = mGUINode.getWorkSpace().getEditorConfig();
        mNodeType    = node.getType();

        switch (mNodeType) {
        case BasicNode :
            initNodeDockPoints();

            break;

        case SuperNode :
            initSuperNodeDockPoints();

            break;
        }
    }

    public void update() {
        switch (mNodeType) {
        case BasicNode :
            initNodeDockPoints();

            break;

        case SuperNode :
            initSuperNodeDockPoints();

            break;
        }
    }

    public Vector<Point> getFreeDockPoints() {
        Vector<Point> points = new Vector<Point>();

        for (DockPoint dp : mDockPoints) {
            if (!dp.mOccupied) {
                points.add(dp.mPos);
            }
        }

        return points;
    }

    public Vector<Point> getOccupiedDockPoints() {
        Vector<Point> points = new Vector<Point>();

        for (DockPoint dp : mDockPoints) {
            if (dp.mOccupied) {
                points.add(dp.mPos);
            }
        }

        return points;
    }

    public synchronized Point occupyDockPointForStartSign() {
        for (DockPoint dp : mDockPoints) {
            if (dp.mType.equalsIgnoreCase(DockPoint.sSTARTSIGN_TYPE)) {

                // TODO remap, if occupied
                dp.use();

                return dp.mPos;
            }
        }

        return null;
    }

    public void releaseDockPointForStartSign() {
        for (DockPoint dp : mDockPoints) {
            if (dp.mType.equals(DockPoint.sSTARTSIGN_TYPE)) {
                dp.release();
            }
        }
    }

    public Point getNearestDockPoint(Edge e, Point p) {
        DockPoint rp          = null;
        int       lastXDist   = -1;
        int       lastYDist   = -1;
        int       actualXDist = -1;
        int       actualYDist = -1;

        for (DockPoint dp : mDockPoints) {
            if (!dp.mOccupied) {
                actualXDist = Math.abs(p.x - dp.mPos.x);
                actualYDist = Math.abs(p.y - dp.mPos.y);

                // Store first free Dockpoint
                if ((lastXDist == -1) && (lastYDist == -1)) {
                    lastXDist = actualXDist;
                    lastYDist = actualYDist;
                    rp        = dp;
                }

                // Store nearest free DockPoint
                if ((actualXDist + actualYDist) < (lastXDist + lastYDist)) {
                    lastXDist = actualXDist;
                    lastYDist = actualYDist;
                    rp        = dp;
                }
            } else {

//              System.out.println("\tdock point (" + dp.mPos.x + "," +dp.mPos.y + ") is occupied!");
            }
        }

        // mark DockPoint as used and store edge
        if (rp != null) {
            rp.use();
            mEdgeDockPoints.put(e, rp);
        } else {

            // System.out.println("Warning! No more docking points available! Edge will not connected!");
        }

        // return a new Point intance that can be altered
        return (rp != null)
               ? (Point) rp.mPos.clone()
               : null;
    }

    /*
     * This method finds a dockpoint for edges that point to the same node
     */
    public Point getNearestSecondDockPoint(Edge e, Point p) {
        DockPoint rp          = null;
        int       lastXDist   = -1;
        int       lastYDist   = -1;
        int       actualXDist = -1;
        int       actualYDist = -1;

        for (DockPoint dp : mDockPoints) {
            if (!dp.mOccupied) {
                actualXDist = Math.abs(p.x - dp.mPos.x);
                actualYDist = Math.abs(p.y - dp.mPos.y);

                // Store first free Dockpoint
                if ((lastXDist == -1) && (lastYDist == -1)) {
                    lastXDist = actualXDist;
                    lastYDist = actualYDist;
                    rp        = dp;
                }

                // Store nearest free DockPoint
                if ((actualXDist + actualYDist) < (lastXDist + lastYDist)) {
                    lastXDist = actualXDist;
                    lastYDist = actualYDist;
                    rp        = dp;
                }
            }
        }

        // mark DockPoint as used and store edge
        if (rp != null) {
            rp.use();
            mEdgeSecondDockPoints.put(e, rp);
        } else {

            // System.out.println("Warning! No more docking points available! Edge will not connected!");
        }

        // return a new Point intance that can be altered
        return (rp != null)
               ? (Point) rp.mPos.clone()
               : null;
    }

    /*
     * Returns the dock point to which the edge is connected
     */
    public Point getDockPoint(Edge e) {
        if (mEdgeDockPoints.containsKey(e)) {

            // return a new Point intance that can be altered
            return (Point) mEdgeDockPoints.get(e).mPos.clone();
        } else {
            return null;
        }
    }

    /*
     * Return the second dock point to which the edge is connected (pointing to
     * the same node
     */
    public Point getSecondDockPoint(Edge e) {
        if (mEdgeSecondDockPoints.containsKey(e)) {

            // return a new Point intance that can be altered
            return (Point) mEdgeSecondDockPoints.get(e).mPos.clone();
        } else {
            return null;
        }
    }

    /*
     * Releases the dock point an edge has used on a node
     *
     * @param Edge that should be released
     *
     */
    public Point freeDockPoint(Edge e) {
        if (mEdgeDockPoints.containsKey(e)) {
            DockPoint dp = mEdgeDockPoints.get(e);

            dp.release();
            mEdgeDockPoints.remove(e);

            // DEBUG
            // System.out.println("Edge " + e.getName() + " dock point disconnected");
            return dp.getPoint();
        } else {
            return null;

            // DEBUG //System.out.println("Edge should be disconnected but never was connected");
        }
    }

    /*
     * Releases the seoncd dock point an edge has used on a node
     *
     * @param Edge that should be released
     *
     */
    public Point freeSecondDockPoint(Edge e) {
        if (mEdgeSecondDockPoints.containsKey(e)) {
            DockPoint dp = mEdgeSecondDockPoints.get(e);

            dp.release();
            mEdgeSecondDockPoints.remove(e);

            // DEBUG
            // System.out.println("Edge "+ e.getName() + " second dock point disconnected");
            return dp.getPoint();
        } else {
            return null;

            // DEBUG //System.out.println("Edge should be disconnected but never was connected");
        }
    }

    /*
     * Return a Set of all connected edges
     */
    public Set<Edge> getConnectedEdges() {
        return mEdgeDockPoints.keySet();
    }

    /*
     *  Removes a connected egde for a dock point and frees the dock point for
     * other edges
     *
     * @param Point from edge which should be released
     */
    private boolean releaseDockPoint(Point p) {
        DockPoint rp          = null;
        int       lastXDist   = -1;
        int       lastYDist   = -1;
        int       actualXDist = -1;
        int       actualYDist = -1;

        for (DockPoint dp : mDockPoints) {
            if (dp.mOccupied) {
                actualXDist = Math.abs(p.x - dp.mPos.x);
                actualYDist = Math.abs(p.y - dp.mPos.y);

                // search
                if ((lastXDist == -1) && (lastYDist == -1)) {
                    lastXDist = actualXDist;
                    lastYDist = actualYDist;
                    rp        = dp;
                }

                // Store nearest free DockPoint
                if ((actualXDist + actualYDist) < (lastXDist + lastYDist)) {
                    lastXDist = actualXDist;
                    lastYDist = actualYDist;
                    rp        = dp;
                }
            }
        }

        // mark DockPoint as free
        if (rp != null) {
            rp.release();
        }

        return (rp != null)
               ? true
               : false;
    }

    private boolean hasDockpoint(String name) {
        for (DockPoint dp : mDockPoints) {
            if (dp.mName.equalsIgnoreCase(name)) {
                return true;
            }
        }

        return false;
    }

    private DockPoint getDockpointByName(String name) {
        for (DockPoint dp : mDockPoints) {
            if (dp.mName.equalsIgnoreCase(name)) {
                return dp;
            }
        }

        return null;
    }

    private void initNodeDockPoints() {
        String dpName   = null;
        double a        = 0.0d;
        double dockXPos = 0.0d;
        double dockYPos = 0.0d;

        for (int cnt = 23; cnt >= 0; cnt--) {
            a        = cnt * Math.PI / 12.0d + (Math.PI);
            dpName   = "dp" + cnt;
            dockXPos = Math.round((Math.sin(a) * 0.5d + 0.5d) * mEditorConfig.sNODEWIDTH);
            dockYPos = Math.round((Math.cos(a) * 0.5d + 0.5d) * mEditorConfig.sNODEHEIGHT);

            if ((dockXPos == 0) && (dockYPos == mEditorConfig.sNODEHEIGHT / 2)) {

                // use most left dockpoint as startsign dockpoint
                if (hasDockpoint(dpName)) {

                    // update dockpoint
                    DockPoint dp = getDockpointByName(dpName);

                    dp.mPos.x = (int) dockXPos;
                    dp.mPos.y = (int) dockYPos;
                } else {

                    // create new dockpoint
                    mDockPoints.add(new DockPoint(dpName, (int) dockXPos, (int) dockYPos, DockPoint.sSTARTSIGN_TYPE));
                }
            } else {
                if (hasDockpoint(dpName)) {

                    // update dockpoint
                    DockPoint dp = getDockpointByName(dpName);

                    dp.mPos.x = (int) dockXPos;
                    dp.mPos.y = (int) dockYPos;
                } else {

                    // create new dockpoint
                    mDockPoints.add(new DockPoint(dpName, (int) dockXPos, (int) dockYPos));
                }
            }
        }
    }

    private void initSuperNodeDockPoints() {
        String dpName   = null;
        double a        = 0.0d;
        double xa       = 0.0d;
        double ya       = 0.0d;
        double fy       = 0.0d;
        double fx       = 0.0d;
        double rh       = mEditorConfig.sNODEHEIGHT / 2.0d;
        double rw       = mEditorConfig.sNODEWIDTH / 2.0d;
        double dockXPos = 0.0d;
        double dockYPos = 0.0d;

        for (int cnt = 24; cnt >= 1; cnt--) {
            dpName = "dp" + cnt;
            a      = cnt * Math.PI / 12 + (Math.PI);
            ya     = Math.cos(a);                  // y-achsenabschnitt
            xa     = Math.sin(a);                  // x-achsenabschnitt

            if (Math.abs(xa) <= Math.abs(ya)) {
                fy       = 1.0d / Math.abs(ya);    // scaling factor for y
                dockYPos = rh * Math.signum(ya) + rh;
                dockXPos = Math.round(xa * fy * rw) + rw;
            } else {
                fx       = 1.0d / Math.abs(xa);    // scaling factor for x
                dockYPos = Math.round(ya * fx * rh) + rh;
                dockXPos = rw * Math.signum(xa) + rw;
            }

            // Debug System.out.println("(x,y)= " + dockXPos + "," + dockYPos);
            if ((dockXPos == 0) && (dockYPos == mEditorConfig.sNODEHEIGHT / 2)) {

                // use most left dockpoint as startsign dockpoint
                if (hasDockpoint(dpName)) {

                    // update dockpoint
                    DockPoint dp = getDockpointByName(dpName);

                    dp.mPos.x = (int) dockXPos;
                    dp.mPos.y = (int) dockYPos;
                } else {

                    // create new dockpoint
                    mDockPoints.add(new DockPoint(dpName, (int) dockXPos, (int) dockYPos, DockPoint.sSTARTSIGN_TYPE));
                }
            } else {
                if (hasDockpoint(dpName)) {

                    // update dockpoint
                    DockPoint dp = getDockpointByName(dpName);

                    dp.mPos.x = (int) dockXPos;
                    dp.mPos.y = (int) dockYPos;
                } else {

                    // create new dockpoint
                    mDockPoints.add(new DockPoint(dpName, (int) dockXPos, (int) dockYPos));
                }
            }
        }
    }

    class DockPoint {
        static final String sSTARTSIGN_TYPE = "start pos dock point";
        String              mName           = null;
        Point               mPos            = null;
        boolean             mOccupied       = false;
        String              mType           = "dock point";

        DockPoint(String name, int x, int y) {
            mName = name;
            mPos  = new Point(x, y);
        }

        DockPoint(String name, int x, int y, String id) {
            mName = name;
            mPos  = new Point(x, y);
            mType = id;
        }

        void use() {
            mOccupied = true;
        }

        void release() {
            mOccupied = false;
        }

        boolean isOccupied() {
            return mOccupied;
        }

        Point getPoint() {
            return (Point) mPos.clone();
        }
    }
}
