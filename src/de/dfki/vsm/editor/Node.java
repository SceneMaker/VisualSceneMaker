package de.dfki.vsm.editor;

import de.dfki.vsm.editor.event.NodeExecutedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.event.NodeStartedEvent;
import de.dfki.vsm.editor.event.NodeTerminatedEvent;
import de.dfki.vsm.editor.util.DockingManager;
import static de.dfki.vsm.editor.util.Preferences.sACTIVITYTRACE;
import static de.dfki.vsm.editor.util.Preferences.sBASIC_NODE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sCEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sEEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sFEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sHISTORY_NODE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sIEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sNODEHEIGHT;
import static de.dfki.vsm.editor.util.Preferences.sNODEWIDTH;
import static de.dfki.vsm.editor.util.Preferences.sPEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sSHOWIDSOFNODES;
import static de.dfki.vsm.editor.util.Preferences.sSTART_SIGN_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sSUPER_NODE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sTEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sVISUALISATION;
import static de.dfki.vsm.editor.util.Preferences.sVISUALISATIONTIME;
import static de.dfki.vsm.editor.util.Preferences.sWORKSPACEFONTSIZE;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Set;
import java.util.Timer;
import java.util.Vector;
import javax.swing.JComponent;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class Node extends JComponent implements EventListener, Observer {

    public enum Type {

        BasicNode,
        SuperNode
    };

    public enum Flavour {

        None,
        ENode,
        TNode,
        CNode,
        PNode,
        INode,
        FNode
    };
    private Type mType;
    private Flavour mFlavour = Flavour.None;
    private de.dfki.vsm.model.sceneflow.Node mDataNode;
    //
    // TODO: move away
    private WorkSpace mWorkSpace;
    /**
     * *************************************************************************
     *
     * Drawing parameters
     *
     *************************************************************************
     */
    // The name which will be displayed on the node
    private String mDisplayName;
    // The color of the node
    // TODO: eventually move computation of color to paint component
    public Color mColor;
    // TODO: move this condition to the data node
    private boolean mIsEndNode = true;
    // ToDO: move to workspace - just have a link here
    private StartSign mStartSign = null;
    private StartSign mAltStartSign = null;
    //private Point mStartSignPosition;
    //
    public DockingManager mDockingManager = null;
    // interaction flags
    public boolean mSelected = false;
    public boolean mPressed = false;
    public boolean mDragged = false;
    //
    private Timer mVisuTimer;
    private VisualisationTask mVisualisationTask;
    private boolean mIsActive;
    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventMulticaster = EventCaster.getInstance();

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public Node(WorkSpace workSpace, de.dfki.vsm.model.sceneflow.Node dataNode) {
        mWorkSpace = workSpace;
        mDataNode = dataNode;
        //
        if (mDataNode instanceof SuperNode) {
            mType = Type.SuperNode;
        } else {
            mType = Type.BasicNode;
        }
        // Init docking manager
        mDockingManager = new DockingManager(this);
        // TODO: move this to data model
        mIsEndNode = (!mDataNode.hasEdge()) ? true : false;
        // check if connected edge(s) is/are cedge(s)
        if (mDataNode.getFlavour().equals(de.dfki.vsm.model.sceneflow.Node.FLAVOUR.CNODE)) {
            // If no additional default edge is present - node is possible end node!
            mIsEndNode = (mDataNode.getDedge() == null) ? true : false;
        }
        // Init the visualization timer
        mVisuTimer = new Timer("Node(" + mDataNode.getId() + ")-Visualization-Timer");

        // Set initial position
        Point pos = new Point(
                mDataNode.getGraphics().getPosition().getXPos(),
                mDataNode.getGraphics().getPosition().getYPos());
        setBounds(pos.x, pos.y, sNODEWIDTH, sNODEHEIGHT);

        // Set the initial start sign
        HashMap<String, de.dfki.vsm.model.sceneflow.Node> startNodeMap
                = mWorkSpace.getSceneFlowManager().getCurrentActiveSuperNode().getStartNodeMap();
        if (startNodeMap.containsKey(mDataNode.getId())) {
            addStartSign();
        }
        if (mDataNode.isHistoryNode()) {
            addAltStartSign();
        }
        // update
        update();
    }

    public Type getType() {
        return mType;
    }

    public void setType(Type type) {
        mType = type;
    }

    public Flavour getFlavour() {
        return mFlavour;
    }

    public void setFlavour(Flavour flavour) {
        mFlavour = flavour;
    }

    public de.dfki.vsm.model.sceneflow.Node getDataNode() {
        return mDataNode;
    }

    public void setDataNode(SuperNode sNode) {
        mDataNode = sNode;
    }

    public boolean containsPoint(int x, int y) {
        return getBounds().contains(x, y);
    }

    public DockingManager getDockingManager() {
        return mDockingManager;
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void update(Observable o, Object obj) {
        //mLogger.message("Node.update(" + obj + ")");
        update();
    }

    private void update() {
        // reset location
        // Recompute the node's docking positions.
        // Free all docking points that have to be
        // recomputed if the node's size has changed
        mDockingManager.update();
        if (mStartSign != null) {
            mStartSign.update();
        }

        if (mAltStartSign != null) {
            mAltStartSign.update();
        }

        /////////////////////////////////////
        // mLogger.message("Node.update()");
        mIsEndNode = (!mDataNode.hasEdge()) ? true : false;
        // check if connected edge(s) is/are cedge(s)
        if (mDataNode.getFlavour().equals(de.dfki.vsm.model.sceneflow.Node.FLAVOUR.CNODE)) {
            // If no additional default edge is present - node is possible end node!
            mIsEndNode = (mDataNode.getDedge() == null) ? true : false;
            ////System.out.println("Is end node " + mIsEndNode);
        }
        /// TODO: wozu das hier?
        if (mDataNode.getFlavour().equals(de.dfki.vsm.model.sceneflow.Node.FLAVOUR.FNODE)) {
            mIsEndNode = false;
        }

        // Update the font and the font metrics that have to be
        // recomputed if the node's font size has changed
        // TODO: Move attributes to preferences and make editable
        Map<TextAttribute, Object> map = new Hashtable<TextAttribute, Object>();
        map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
        map.put(TextAttribute.FAMILY, Font.SANS_SERIF);
        map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
        map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
        map.put(TextAttribute.SIZE, sWORKSPACEFONTSIZE);
        // Derive the font from the attribute map
        Font font = Font.getFont(map);
        // Derive the node's font metrics from the font
        FontMetrics fontMetrics = getFontMetrics(font);
        // Set the node's font to the updated font
        setFont(font);

        // Update the display name that has to be changed if the
        // node's size or the node's font size have chaged
        String prefix = "";
        if (fontMetrics.stringWidth(mDataNode.getName()) > (sNODEWIDTH - 10)) {
            for (char c : mDataNode.getName().toCharArray()) {
                if (fontMetrics.stringWidth(prefix + c + "...") < sNODEWIDTH - 10) {
                    prefix += c;
                } else {
                    break;
                }

            }
            mDisplayName = prefix + "...";
        } else {
            mDisplayName = mDataNode.getName();
        }

        // Update the color of the node that has to be changed
        // if the type or the flavour of the node have changed
        switch (mType) {
            case SuperNode:
                mColor = sSUPER_NODE_COLOR;
                break;
            case BasicNode:
                mColor = sBASIC_NODE_COLOR;
                break;
        }
        // Set the history node color
        if (mDataNode.isHistoryNode()) {
            mColor = sHISTORY_NODE_COLOR;
        }
        // Set the flavour dependend color
        switch (mFlavour) {
            case ENode:
                mColor = sEEDGE_COLOR;
                break;
            case FNode:
                mColor = sFEDGE_COLOR;
                break;
            case TNode:
                mColor = sTEDGE_COLOR;
                break;
            case PNode:
                mColor = sPEDGE_COLOR;
                break;
            case CNode:
                mColor = sCEDGE_COLOR;
                break;
            case INode:
                mColor = sIEDGE_COLOR;
                break;
        }

        // Update the bounds if the node's size has changed
        setBounds(getX(), getY(), sNODEWIDTH, sNODEHEIGHT);
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void update(EventObject event) {
        if (sVISUALISATION) {
            if (event instanceof NodeStartedEvent) {
                if ((((NodeStartedEvent) event).getNode().equals(mDataNode))
                        || ((NodeStartedEvent) event).getNode().isSubNodeOf(mDataNode)) {
                    // Cancel the visualization the previous
                    if (mVisualisationTask != null) {
                        mVisualisationTask.cancel();
                    }
                    mIsActive = true;
                    // TODO: necessary?
                    repaint();
                }
            } else if (event instanceof NodeExecutedEvent) {
                if ((((NodeExecutedEvent) event).getNode().equals(mDataNode))
                        || ((NodeExecutedEvent) event).getNode().isSubNodeOf(mDataNode)) {
                    mIsActive = false;
                    if (mVisualisationTask != null) {
                        mVisualisationTask.cancel();
                    }
                    mVisualisationTask = new VisualisationTask(sVISUALISATIONTIME, this);
                    mVisuTimer.schedule(mVisualisationTask, 0, 15);
                }
            } else if (event instanceof NodeTerminatedEvent) {
                mIsActive = false;
                if ((((NodeTerminatedEvent) event).getNode().equals(mDataNode))
                        || ((NodeTerminatedEvent) event).getNode().isSubNodeOf(mDataNode)) {
                    if (mVisualisationTask != null) {
                        mVisualisationTask.cancel();
                    }
                    mVisualisationTask = new VisualisationTask(sVISUALISATIONTIME, this, new Color(0, 0, 0, 100));
                    mVisuTimer.schedule(mVisualisationTask, 0, 15);
                }
            }
        }
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    //tODO: why synchronized??
    public void resetLocation(Point newLocation) {
        Point location = getLocation();
        for (Edge edge : mDockingManager.getConnectedEdges()) {
            edge.mEg.updateRealtiveEdgeControlPointPos(
                    this,
                    newLocation.x - location.x,
                    newLocation.y - location.y);
        }
        setLocation(newLocation);
    }
    //tODO: why synchronized??

    public void updateLocation(Point vector) {
        Point location = getLocation();
        for (Edge edge : mDockingManager.getConnectedEdges()) {
            edge.mEg.updateRealtiveEdgeControlPointPos(this, vector.x, vector.y);
        }
        setLocation(location.x + vector.x, location.y + vector.y);

        updateDataModel();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    // TODO - move to controler class - sceneflowManager!
    private void updateDataModel() {
//        mDataNode.getGraphics().setPosition(getLocation().x, getLocation().y);
        de.dfki.vsm.model.sceneflow.graphics.node.Graphics g
                = new de.dfki.vsm.model.sceneflow.graphics.node.Graphics(getLocation().x, getLocation().y);
        mDataNode.setGraphics(g);
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    // TODO: move to workspace
    public void removeStartSign() {
        if (mStartSign != null) {
            mDockingManager.releaseDockPointForStartSign();
            mWorkSpace.remove(mStartSign);
            mWorkSpace.mObservable.deleteObserver(mStartSign);
            mStartSign = null;
        }

//    if (mAltStartSign != null) {
//      mDockingManager.releaseDockPointForStartSign();
//      mWorkSpace.remove(mAltStartSign);
//      mAltStartSign = null;
//    }
    }

    // TODO: move to workspace
    public void addStartSign() {
        Point p = mDockingManager.occupyDockPointForStartSign();
        mStartSign = new StartSign(this, p);
        mWorkSpace.add(mStartSign);
        mWorkSpace.mObservable.addObserver(mStartSign);
    }

    public void addAltStartSign() {
        Point p = mDockingManager.occupyDockPointForStartSign();
        mAltStartSign = new StartSign(this, p, true, Color.LIGHT_GRAY);
        mWorkSpace.add(mAltStartSign);
        mWorkSpace.mObservable.addObserver(mAltStartSign);
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    // Tells the node that an edge connects and that node is sourcenode
    public Point connectEdgeAtSourceNode(Edge edge, Point point) {
        mIsEndNode = false;

        // get location of node
        Point loc = getLocation();
        // get relative (to the current node) coordinates;
        point.setLocation(point.x - loc.x, point.y - loc.y);
        Point dp = mDockingManager.getNearestDockPoint(edge, point);

        // make position absolute to underlying canvas
        dp.setLocation(dp.x + loc.x, dp.y + loc.y);

        // set working type and color
        switch (edge.getType()) {
            case EEDGE:
                mFlavour = (mFlavour == Flavour.None) ? Flavour.ENode : mFlavour;
                mColor = (mFlavour == Flavour.ENode) ? sEEDGE_COLOR : mColor;
                break;
            case FEDGE:
                mFlavour = (mFlavour == Flavour.None) ? Flavour.FNode : mFlavour;
                mColor = (mFlavour == Flavour.FNode) ? sFEDGE_COLOR : mColor;
                break;
            case TEDGE:
                mFlavour = (mFlavour == Flavour.None) ? Flavour.TNode : mFlavour;
                mColor = (mFlavour == Flavour.TNode) ? sTEDGE_COLOR : mColor;
                break;
            case CEDGE:
                mFlavour = Flavour.CNode;
                mColor = sCEDGE_COLOR;
                break;
            case PEDGE:
                mFlavour = Flavour.PNode;
                mColor = sPEDGE_COLOR;
                break;
            case IEDGE:
                mFlavour = Flavour.INode;
                mColor = sIEDGE_COLOR;
                break;
        }
        return dp;
    }

    // Tells the node that an edge connects
    public Point connectEdgetAtTargetNode(Edge e, Point p) {
        // get location of node
        Point loc = getLocation();
        // get relative (to the current node) coordinates;
        p.setLocation(p.x - loc.x, p.y - loc.y);
        Point dp = mDockingManager.getNearestDockPoint(e, p);

        // make position absolute to underlying canvas
        dp.setLocation(dp.x + loc.x, dp.y + loc.y);

        return dp;
    }

    public Point connectSelfPointingEdge(Edge e, Point p) {
        Point loc = getLocation();
        // get relative (to the current node) coordinates;
        p.setLocation(p.x - loc.x, p.y - loc.y);
        Point dp = mDockingManager.getNearestSecondDockPoint(e, p);

        // make position absolute to underlying canvas
        dp.setLocation(dp.x + loc.x, dp.y + loc.y);

        return dp;
    }

    public Point disconnectEdge(Edge e) {
        Point relPos = mDockingManager.freeDockPoint(e);
        Point pos = getLocation();
        Point absLoc;
        
        if(relPos!=null){
            absLoc = new Point(relPos.x + pos.x, relPos.y + pos.y);
        }
        else{
            absLoc = new Point(pos.x, pos.y);
        }

        return absLoc;
    }

    public Point disconnectSelfPointingEdge(Edge e) {
        Point relPos = mDockingManager.freeSecondDockPoint(e);
        Point pos = getLocation();
        Point absLoc = new Point(relPos.x + pos.x, relPos.y + pos.y);

        return absLoc;
    }

    /*
     * Returns the center of a node
     */
    public Point getCenterPoint() {
        Point loc = getLocation();
        Point c = new Point();
        c.setLocation(loc.x + (sNODEWIDTH / 2), loc.y + (sNODEHEIGHT / 2));
        return c;
    }

    public Set<Edge> getConnectedEdges() {
        return mDockingManager.getConnectedEdges();
    }

    public Point getEdgeDockPoint(Edge e) {
        Point loc = getLocation();

        Point dp = mDockingManager.getDockPoint(e);

        // make position absolute to underlying canvas        
        if(dp!=null){
             dp.setLocation(dp.x + loc.x, dp.y + loc.y);
        }
        else{
            if(this.mIsEndNode)
                return (new Point(loc.x, loc.y + getHeight()/2));
            else
                return (new Point(loc.x + getWidth(), loc.y + getHeight()/2));
        }
      
        return dp;
    }

    public Point getSelfPointingEdgeDockPoint(Edge e) {
        Point loc = getLocation();

        Point dp = mDockingManager.getSecondDockPoint(e);

        // make position absolute to underlying canvas
        if(dp!=null){
             dp.setLocation(dp.x + loc.x, dp.y + loc.y);
        }
        else{
            if(this.mIsEndNode)
                return (new Point(loc.x, loc.y + getHeight()/2));
            else
                return (new Point(loc.x + getWidth(), loc.y + getHeight()/2));
        }

        return dp;
    }

    public Vector<Point> getEdgeStartPoints() {
        Vector<Point> fDP = mDockingManager.getFreeDockPoints();
        Vector<Point> points = new Vector<Point>();

        Point loc = getLocation();

        for (Point p : fDP) {
            // make position absolute to underlying canvas
            points.add(new Point(p.x + loc.x, p.y + loc.y));
        }

        return points;
    }

    public boolean isEdgeAllowed(Edge.TYPE eType) {
        boolean allowed = false;

        switch (mFlavour) {
            case None: // if node working type is unclear, allow all (except iedge for nodes)
                allowed = (mType == Type.BasicNode && eType == Edge.TYPE.IEDGE) ? false : true;
                break;
            case ENode: // only one eegde is allowed
                allowed = false;
                break;
            case TNode: // only one tegde is allowed
                allowed = false;
                break;
            case CNode: // only cedges are allowed - TODO allow dedge/tedge
                allowed = (eType == Edge.TYPE.CEDGE || ((mDataNode.getDedge() == null) && ((eType == Edge.TYPE.TEDGE || (eType == Edge.TYPE.EEDGE))))) ? true : false;
                break;
            case PNode: // only pedges are allowed - TODO allow dedge/tedge
                allowed = (eType == Edge.TYPE.PEDGE) ? true : false;
                break;
            case FNode: // only fedges are allowed
                allowed = (eType == Edge.TYPE.FEDGE) ? true : false;
                break;
            case INode: // only iedges are allowed
                allowed = (eType == Edge.TYPE.IEDGE) ? true : false;
                break;
        }
        return allowed;
    }

    /*
     * Resets the node to its default visual behavior
     */
    public void setDeselected() {
        mSelected = false;
        mPressed = false;
        mDragged = false;
        repaint();
    }

    public void mouseClicked(MouseEvent event) {
        mPressed = false;
        mSelected = true;
        Point loc = getLocation();
        Point clickLoc = event.getPoint();
        //mLastMousePosition = new Point(clickLoc);
        // save click location relavitvely to node postion
        //mClickPosition.setLocation(clickLoc.x - loc.x, clickLoc.y - loc.y);
        repaint();
// enter supernode, if it has been double clicked

        //TODO: move to workspace
        if (event.getButton() == MouseEvent.BUTTON1 && event.getClickCount() == 2) {
            if (mType == Type.SuperNode) {
                mWorkSpace.increaseWorkSpaceLevel(this);
            }

        }
        // show contect menu
        //TODO: move to workspace
        if (event.getButton() == MouseEvent.BUTTON3 && event.getClickCount() == 1) {
            mWorkSpace.showContextMenu(event, this);
        }

//////////!!!!!!!!!!!!!!!!!!!!
        //System.err.println("Sending node selected event");
        mEventMulticaster.convey(new NodeSelectedEvent(this, this.getDataNode()));
    }

    public void mousePressed(MouseEvent event) {
        mPressed = true;
        mSelected = true;

        Point loc = getLocation();
        Point clickLoc = event.getPoint();
        // mLastMousePosition =                new Point(clickLoc);
        // save click location relavitvely to node postion
        //mClickPosition.setLocation(clickLoc.x - loc.x, clickLoc.y - loc.y);
        repaint();
// enter supernode, if it has been double clicked
        //TODO: move to workspace
        if (event.getButton() == MouseEvent.BUTTON1 && event.getClickCount() == 2) {
            if (mType == Type.SuperNode) {
                mWorkSpace.increaseWorkSpaceLevel(this);
            }

        }
        // show contect menu
        if (event.getButton() == MouseEvent.BUTTON3 && event.getClickCount() == 1) {
            mWorkSpace.showContextMenu(event, this);
        }
    }

    public void mouseReleased(MouseEvent e) {
        mPressed = false;
        mDragged = false;
        repaint();
    }

    public void stopVisualisation() {
        mVisuTimer.purge();
        mVisuTimer.cancel();
        // TODO: why null?
        mVisuTimer = null;
    }

    public void hightlightNode() {
        if (mVisuTimer != null) {
            if (mVisualisationTask != null) {
                mVisualisationTask.cancel();
            }

            mVisualisationTask = new VisualisationTask(sVISUALISATIONTIME, this, new Color(255, 255, 255, 100), VisualisationTask.Type.Highlight);
            mVisuTimer.schedule(mVisualisationTask, 0, 25);
        }

    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    @Override
    public void paintComponent(Graphics graphics) {
        final Graphics2D graphics2D = (Graphics2D) graphics;
        // TODO move to update
        // Compute the font metrics and the correction offsets
        final FontMetrics fontMetrics = getFontMetrics(getFont());
        final int hOffset = (fontMetrics.getAscent() - fontMetrics.getDescent()) / 2;
        final int wIdOffset = fontMetrics.stringWidth("[" + mDataNode.getId() + "]") / 2;
        final int wNameOffset = fontMetrics.stringWidth(mDisplayName) / 2;
        // Compute the border which is relative to a nodes size.
        // It is used for visualising an end nodes and node selection
        final float borderSize = Math.max(1.0f, sNODEWIDTH / 25.0f);
        final int borderOffset = Math.round(borderSize);
        final float[] dashPattern = {borderSize * 0.5f, borderSize * 1.25f};
        // Enable antialiasing
        graphics2D.setRenderingHint(
                RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        // Set the color to gray while pressed
        if (mPressed) {
            graphics2D.setColor(Color.GRAY);
        } else {
            graphics2D.setColor(mColor);
        }
        // Draw the node as a supernode
        if (mType == Type.SuperNode) {
            graphics2D.fillRect(
                    borderOffset + 1,
                    borderOffset + 1,
                    sNODEWIDTH - borderOffset * 2 - 1,
                    sNODEHEIGHT - borderOffset * 2 - 1);
            if (mSelected) {
                graphics2D.setStroke(new BasicStroke(
                        borderSize,
                        BasicStroke.CAP_SQUARE,
                        BasicStroke.JOIN_MITER,
                        10, dashPattern, 0));
                graphics2D.setColor(sSTART_SIGN_COLOR);
                graphics2D.drawRect(
                        borderOffset,
                        borderOffset,
                        sNODEWIDTH - borderOffset * 2,
                        sNODEHEIGHT - borderOffset * 2);
            } else if (mIsEndNode) {
                graphics2D.setStroke(new BasicStroke(borderSize));
                graphics2D.setColor(mColor.darker());
                graphics2D.drawRect(
                        borderOffset + 1,
                        borderOffset + 1,
                        sNODEWIDTH - borderOffset * 2 - 2,
                        sNODEHEIGHT - borderOffset * 2 - 2);
            }
            // Draw visualization highlights
            if (mIsActive) {
                graphics2D.setColor(new Color(246, 0, 0, 100));
                graphics2D.fillRect(1, 1, sNODEWIDTH - 1, sNODEHEIGHT - 1);
            }

            if (mVisualisationTask != null) {
                if (mVisualisationTask.getActivityTime() > 10) {
                    graphics2D.setColor(mVisualisationTask.getColor());
                } else {
                    int red = mVisualisationTask.getColor().getRed();
                    int green = mVisualisationTask.getColor().getGreen();
                    int blue = mVisualisationTask.getColor().getBlue();
                    int alpha = mVisualisationTask.getColor().getAlpha();
                    int gray = ((10 - mVisualisationTask.getActivityTime()) * 6);
                    graphics2D.setColor(new Color((sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? gray : red, (sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? gray : green, (sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? gray : blue, (sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? 100 : alpha - (alpha - 6 * mVisualisationTask.getActivityTime())));
                }
                graphics2D.fillRect(1, 1, sNODEWIDTH - 1, sNODEHEIGHT - 1);
            }
        } else if (mType == Type.BasicNode) {
            graphics2D.fillOval(
                    borderOffset + 1,
                    borderOffset + 1,
                    sNODEWIDTH - borderOffset * 2 - 1,
                    sNODEHEIGHT - borderOffset * 2 - 1);
            if (mSelected) {
                graphics2D.setStroke(new BasicStroke(
                        borderSize,
                        BasicStroke.CAP_ROUND,
                        BasicStroke.JOIN_MITER,
                        2, dashPattern, 0));
                // TODO: warum andrs als bei supernode?
                graphics2D.setColor(sSTART_SIGN_COLOR);
                graphics2D.drawOval(
                        borderOffset,
                        borderOffset,
                        sNODEWIDTH - borderOffset * 2,
                        sNODEHEIGHT - borderOffset * 2);
            } else if (mIsEndNode) {
                graphics2D.setStroke(new BasicStroke(borderSize));
                graphics2D.setColor(mColor.darker());
                graphics2D.drawOval(
                        borderOffset + 1,
                        borderOffset + 1,
                        sNODEWIDTH - borderOffset * 2 - 2,
                        sNODEHEIGHT - borderOffset * 2 - 2);
            }
            // draw activity cue
            if (mIsActive) {
                graphics2D.setColor(new Color(246, 0, 0, 100));
                graphics2D.fillOval(1, 1, sNODEWIDTH - 1, sNODEHEIGHT - 1);
            }
            // draw visualisation ...
            if (mVisualisationTask != null) {
                if (mVisualisationTask.getActivityTime() <= 10) {             // fade out
                    int red = mVisualisationTask.getColor().getRed();
                    int green = mVisualisationTask.getColor().getGreen();
                    int blue = mVisualisationTask.getColor().getBlue();
                    int alpha = mVisualisationTask.getColor().getAlpha();
                    int gray = ((10 - mVisualisationTask.getActivityTime()) * 6);
                    graphics2D.setColor(new Color((sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? gray : red, (sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? gray : green, (sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? gray : blue, (sACTIVITYTRACE && !mVisualisationTask.isHighLight()) ? 100 : alpha - (alpha - 6 * mVisualisationTask.getActivityTime())));
                } else {
                    graphics2D.setColor(mVisualisationTask.getColor());
                }
                graphics2D.setStroke(new BasicStroke(20f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
                graphics2D.fillOval(1, 1, sNODEWIDTH - 1, sNODEHEIGHT - 1);
            }
        }
        // Draw the node's display name
        if (mDataNode.isHistoryNode()) {
            graphics2D.setColor(Color.BLACK);
        } else {
            graphics2D.setColor(Color.WHITE);
        }
        if (!mDisplayName.isEmpty()) {
            graphics2D.drawString(
                    mDisplayName,
                    sNODEWIDTH / 2 - wNameOffset,
                    (sNODEHEIGHT + 2) / 2 + hOffset);
        }
        // Draw the node's identifier string
        if (sSHOWIDSOFNODES) {
            graphics2D.setColor(Color.LIGHT_GRAY);
            graphics2D.drawString(
                    "[" + mDataNode.getId() + "]",
                    sNODEWIDTH / 2 - wIdOffset,
                    ((sNODEHEIGHT + 2) / 2) + hOffset + fontMetrics.getHeight());
        }
    }
}
