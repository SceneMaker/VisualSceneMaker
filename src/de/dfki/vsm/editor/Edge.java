package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.event.EdgeExecutedEvent;
import de.dfki.vsm.editor.event.EdgeSelectedEvent;
import de.dfki.vsm.editor.util.EdgeGraphics;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import static de.dfki.vsm.editor.util.Preferences.sCEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sEEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sFEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sIEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sPEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sTEDGE_COLOR;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.font.TextAttribute;
import java.awt.geom.AffineTransform;

import java.util.Hashtable;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Timer;

import javax.swing.JComponent;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class Edge extends JComponent implements EventListener, Observer, MouseListener{
        
    // The actual type
    private TYPE mType = null;

    // Reference to data model edges and nodes
    private de.dfki.vsm.model.sceneflow.Edge mDataEdge = null;

    // The two graphical nodes to which this edge is connected
    private Node        mSourceNode               = null;
    private Node        mTargetNode               = null;
    private boolean     hasAlternativeTargetNodes = false;
    private boolean     mPointingToSameNode       = false;
    public EdgeGraphics mEg                       = null;
    private WorkSpace   mWorkSpace                = null;

    // rendering issues
    private FontMetrics mFM                   = null;
    private int         mFontWidthCorrection  = 0;
    private int         mFontHeightCorrection = 0;

    // For mouse interaction ...
    public boolean mIsSelected = false;

    // edge control points
    public boolean mCP1Selected = false;
    public boolean mCP2Selected = false;

    // start an end points of edge
    public boolean mCEPSelected = false;
    public boolean mCSPSelected = false;

    // last dockpoints
    public Point mLastTargetNodeDockPoint = null;

    // Activity monitor
    private VisualisationTask      mVisualisationTask = null;
    private final LOGDefaultLogger mLogger            = LOGDefaultLogger.getInstance();
    private final EventCaster      mEventMulticaster  = EventCaster.getInstance();

    //

    // other stuff
    private String             mName;
    private String             mDescription;
    private Color              mColor;
    private ProjectPreferences mPreferences;
    private Timer              mVisualisationTimer;

    public enum TYPE {
        EEDGE, TEDGE, CEDGE, PEDGE, IEDGE, FEDGE
    }

    ;
    public Edge(TYPE type) {

        // TODO: remove constructor
        switch (type) {
        case EEDGE :
            mType        = TYPE.EEDGE;
            mName        = "Epsilon";
            mDescription = "Conditionless edge";
            mColor       = sEEDGE_COLOR;

            break;

        case FEDGE :
            mType        = TYPE.FEDGE;
            mName        = "Fork";
            mDescription = "Fork edge";
            mColor       = sFEDGE_COLOR;

            break;

        case TEDGE :
            mType        = TYPE.TEDGE;
            mName        = "Timeout";
            mDescription = "Edge with a time condition";
            mColor       = sTEDGE_COLOR;

            break;

        case CEDGE :
            mType        = TYPE.CEDGE;
            mName        = "Conditional";
            mDescription = "Edge with a logical condition";
            mColor       = sCEDGE_COLOR;

            break;

        case PEDGE :
            mType        = TYPE.PEDGE;
            mName        = "Probability";
            mDescription = "Edge with a probalistic condition";
            mColor       = sPEDGE_COLOR;

            break;

        case IEDGE :
            mType        = TYPE.IEDGE;
            mName        = "Interruptive";
            mDescription = "Edge with a logical condition that interrupts supernodes";
            mColor       = sIEDGE_COLOR;

            break;
        }
    }

    public Edge(WorkSpace ws, de.dfki.vsm.model.sceneflow.Edge edge, TYPE type, Node sourceNode, Node targetNode,
                ProjectPreferences preferences) {
        mDataEdge           = edge;
        mWorkSpace          = ws;
        mPreferences        = preferences;
        mSourceNode         = sourceNode;
        mTargetNode         = targetNode;
        mType               = type;
        mPointingToSameNode = (mTargetNode == mSourceNode)
                              ? true
                              : false;

        // Timer
        mVisualisationTimer = new Timer("Edge(" + mDataEdge.getSource() + "->" + mDataEdge.getTarget()
                                        + ")-Visualization-Timer");
        update();
        mEg = new EdgeGraphics(this, null, null);
        setVisible(true);
    }

    // TODO: Neuer Konstruktor, der Source und Target dockpoint "mitbekommt"
    public Edge(WorkSpace ws, de.dfki.vsm.model.sceneflow.Edge edge, TYPE type, Node sourceNode, Node targetNode,
                Point sourceDockPoint, Point targetDockpoint) {
        mDataEdge           = edge;
        mWorkSpace          = ws;
        mPreferences        = ws.getPreferences();
        mSourceNode         = sourceNode;
        mTargetNode         = targetNode;
        mType               = type;
        mPointingToSameNode = (mTargetNode == mSourceNode)
                              ? true
                              : false;

        // Timer
        mVisualisationTimer = new Timer("Edge(" + mDataEdge.getSource() + "->" + mDataEdge.getTarget()
                                        + ")-Visualization-Timer");
        update();
        mEg = new EdgeGraphics(this, sourceDockPoint, targetDockpoint);
        setVisible(true);
    }

    public void update(Observable o, Object obj) {

        // mLogger.message("Edge.update(" + obj + ")");
        update();
    }

    public de.dfki.vsm.model.sceneflow.Edge getDataEdge() {
        return mDataEdge;
    }

    public Node getSourceNode() {
        return mSourceNode;
    }

    public Node getTargetNode() {
        return mTargetNode;
    }

    public TYPE getType() {
        return mType;
    }

    @Override
    public String getName() {
        return mName + "(" + mSourceNode.getDataNode().getId() + "->" + mTargetNode.getDataNode().getId() + ")";
    }

    public String getDescription() {
        return mDescription;
    }

    public void update() {

        // configure type
        if (mDataEdge != null) {
            switch (mType) {
            case EEDGE :
                mName        = "Epsilon";
                mColor       = sEEDGE_COLOR;
                mDescription = "";

                break;

            case FEDGE :
                mName        = "Fork";
                mColor       = sFEDGE_COLOR;
                mDescription = "";

                break;

            case TEDGE :
                mName        = "Timeout";
                mColor       = sTEDGE_COLOR;
                mDescription = ((TEdge) mDataEdge).getTimeout() + "ms";

                break;

            case CEDGE :
                mName  = "Conditional";
                mColor = sCEDGE_COLOR;

                if (((CEdge) mDataEdge).getCondition() != null) {
                    mDescription = ((CEdge) mDataEdge).getCondition().getConcreteSyntax();
                } else {
                    mDescription = "";
                }

                break;

            case PEDGE :
                mName        = "Propabilistic";
                mColor       = sPEDGE_COLOR;
                mDescription = ((PEdge) mDataEdge).getProbability() + "%";

                break;

            case IEDGE :
                mName  = "Interruptive";
                mColor = sIEDGE_COLOR;

                if (((IEdge) mDataEdge).getCondition() != null) {
                    mDescription = ((IEdge) mDataEdge).getCondition().getConcreteSyntax();
                } else {
                    mDescription = "";
                }

                break;
            }

            hasAlternativeTargetNodes = !mDataEdge.getAltStartNodeMap().isEmpty();
        }

        // Update the font and the font metrics that have to be
        // recomputed if the node's font size has changed
        // TODO: Move attributes to preferences and make editable
        Map<TextAttribute, Object> map = new Hashtable<TextAttribute, Object>();

        map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
        map.put(TextAttribute.FAMILY, Font.SANS_SERIF);

        // map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
        map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
        map.put(TextAttribute.SIZE, mPreferences.sWORKSPACEFONTSIZE);

        // Derive the font from the attribute map
        Font font = Font.getFont(map);

        // Derive the node's font metrics from the font
        mFM = getFontMetrics(font);

        // Set the edge's font to the updated font
        setFont(font);
        mFontWidthCorrection  = mFM.stringWidth(mName) / 2;
        mFontHeightCorrection = (mFM.getAscent() - mFM.getDescent()) / 2;
    }

    public void setDeselected() {
        mIsSelected  = false;
        mCP1Selected = false;
        mCP2Selected = false;
        mCSPSelected = false;
        mCEPSelected = false;
        repaint();
    }

    public void mouseClicked(java.awt.event.MouseEvent event) {

        //////////!!!!!!!!!!!!!!!!!!!!
        // PG: 25.2.11 DISABELD: System.err.println("Sending node selected event");
        EventCaster.getInstance().convey(new EdgeSelectedEvent(this, this.getDataEdge()));
        mIsSelected = true;

        if (mEg.controlPoint1HandlerContainsPoint(event.getPoint(), 10)) {
            mCP1Selected = true;
            mCP2Selected = false;
            mCEPSelected = false;
            mCSPSelected = false;
        }

        if (mEg.controlPoint2HandlerContainsPoint(event.getPoint(), 10)) {
            mCP1Selected = false;
            mCP2Selected = true;
            mCEPSelected = false;
            mCSPSelected = false;
        }

        if (!(mCP1Selected || mCP2Selected) && mEg.curveStartPointContainsPoint(event.getPoint(), 10)) {
            mCP1Selected = false;
            mCP2Selected = false;
            mCEPSelected = false;
            mCSPSelected = true;
        }

        if (!(mCP1Selected || mCP2Selected) && mEg.curveEndPointContainsPoint(event.getPoint(), 10)) {
            mCP1Selected = false;
            mCP2Selected = false;
            mCEPSelected = true;
            mCSPSelected = false;
        }

        // showActivity();
//      revalidate();
        repaint();

        // show contect menu
        if ((event.getButton() == MouseEvent.BUTTON3) && (event.getClickCount() == 1)) {
            mWorkSpace.showContextMenu(event, this);
        }
    }

    /*
     * Handles the mouse pressed event
     */
    public void mousePressed(java.awt.event.MouseEvent e) {
        mIsSelected = true;

        if (mEg.controlPoint1HandlerContainsPoint(e.getPoint(), 10)) {
            mCP1Selected = true;
            mCP2Selected = false;
            mCEPSelected = false;
            mCSPSelected = false;
        }

        if (mEg.controlPoint2HandlerContainsPoint(e.getPoint(), 10)) {
            mCP1Selected = false;
            mCP2Selected = true;
            mCEPSelected = false;
            mCSPSelected = false;
        }

        if (!(mCP1Selected || mCP2Selected) && mEg.curveStartPointContainsPoint(e.getPoint(), 10)) {
            mCP1Selected = false;
            mCP2Selected = false;
            mCEPSelected = false;
            mCSPSelected = true;
        }

        if (!(mCP1Selected || mCP2Selected) && mEg.curveEndPointContainsPoint(e.getPoint(), 10)) {
            mCP1Selected = false;
            mCP2Selected = false;
            mCEPSelected = true;
            mCSPSelected = false;
        }

        // revalidate();
        repaint();
    }

    public void mouseReleased(java.awt.event.MouseEvent e) {

        // System.out.println("edge - mouse released");
        if (mCSPSelected) {
            Point relPos = (Point) mSourceNode.getLocation().clone();

            relPos.setLocation(e.getX() - relPos.x, e.getY() - relPos.y);

            // DEBUG System.out.println("set new dock point for pos " + relPos);
            mSourceNode.mDockingManager.freeDockPoint(this);
            mSourceNode.mDockingManager.getNearestDockPoint(this, relPos);
        }

        if (mCEPSelected) {
            Point relPos = (Point) mTargetNode.getLocation().clone();

            relPos.setLocation(e.getX() - relPos.x, e.getY() - relPos.y);

            // DEBUG System.out.println("set new dock point for pos " + relPos);
            if (!mPointingToSameNode) {
                mTargetNode.mDockingManager.freeDockPoint(this);
                mTargetNode.mDockingManager.getNearestDockPoint(this, relPos);
            } else {
                mTargetNode.mDockingManager.freeSecondDockPoint(this);
                mTargetNode.mDockingManager.getNearestSecondDockPoint(this, relPos);
            }
        }

        mCP1Selected = false;
        mCP2Selected = false;
        mCEPSelected = false;
        mCSPSelected = false;

        // revalidate();
        repaint();
    }

    public void mouseDragged(java.awt.event.MouseEvent e) {
        Point p = e.getPoint();

        // do not allow x and y values below 10
        if (p.x - 10 < 0) {
            p.x = 10;
        }

        if (p.y - 10 < 0) {
            p.y = 10;
        }

        if (mCP1Selected) {
            mEg.mCCrtl1.setLocation(p);
        }

        if (mCP2Selected) {
            mEg.mCCrtl2.setLocation(p);
        }

        if (mCEPSelected) {
            if (!mPointingToSameNode) {
                mLastTargetNodeDockPoint = mTargetNode.mDockingManager.freeDockPoint(this);
            } else {
                mLastTargetNodeDockPoint = mTargetNode.mDockingManager.freeSecondDockPoint(this);
            }

            mEg.mAbsoluteEndPos.setLocation(p);
        }

        if (mCSPSelected) {
            mSourceNode.mDockingManager.freeDockPoint(this);

            // TODO store last start /end node and start and end pos
            mEg.mAbsoluteStartPos.setLocation(p);
        }

        // revalidate();
        repaint();
    }

    public void mouseEntered(java.awt.event.MouseEvent e) {}

    public void mouseExited(java.awt.event.MouseEvent e) {}

    public void mouseMoved(java.awt.event.MouseEvent e) {}

    public void straightenEdge() {
        mEg.initCurve();
    }

    public void rebuildEdgeNicely() {

        // disconnectEdge
        if (!mPointingToSameNode) {
            mTargetNode.mDockingManager.freeDockPoint(this);
        } else {
            mTargetNode.mDockingManager.freeSecondDockPoint(this);
        }

        mSourceNode.mDockingManager.freeDockPoint(this);
        mEg.initEdgeGraphics(this, null, null);
    }

    private void paintRoundedTextBadge(Graphics2D graphics, Point position, String text) {
        mFontWidthCorrection = mFM.stringWidth(text) / 2;

        // do an exact font positioning

        FontRenderContext renderContext  = graphics.getFontRenderContext();
        GlyphVector       glyphVector    = getFont().createGlyphVector(renderContext, text);
        Rectangle         visualBounds   = glyphVector.getVisualBounds().getBounds();
        int               halfTextHeight = visualBounds.height / 2;
        int               textY          = position.y - visualBounds.height / 2 - visualBounds.y;

        graphics.setColor(Color.WHITE);
        graphics.fillRect(position.x - mFontWidthCorrection - 2, position.y - halfTextHeight - 2,
                          mFontWidthCorrection * 2 + 4, halfTextHeight * 2 + 4);
        graphics.setColor(mColor);
        graphics.setStroke(new BasicStroke(0.5f));
        graphics.drawRoundRect(position.x - mFontWidthCorrection - 2, position.y - halfTextHeight - 2,
                               mFontWidthCorrection * 2 + 4, halfTextHeight * 2 + 4, 5, 5);
        graphics.setColor(mColor.darker());
        graphics.drawString(text, position.x - mFontWidthCorrection, textY);
    }
        
    @Override
    public void paintComponent(java.awt.Graphics g) {
        Graphics2D graphics = (Graphics2D) g;

//      if (mWorkSpace != null) {
//        if (mEg.mEdge != null) {
        mEg.updateDrawingParameters();

        Rectangle bounds = getBounds();

        // translate absolute to relative coordinates
        // graphics.translate(-bounds.x, -bounds.y);

        // Debug
//      graphics.setColor(Color.BLACK);
//      graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
//      graphics.setStroke(new BasicStroke(0.5f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
//      graphics.drawRect(bounds.x, bounds.y, bounds.width - 1, bounds.height - 1);
        graphics.setColor(mColor);
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics.setStroke(new BasicStroke(mPreferences.sNODEWIDTH / 30.0f, BasicStroke.CAP_BUTT,
                                           BasicStroke.JOIN_MITER));
        graphics.draw(mEg.mCurve);

        if (mDescription.length() > 0) {
            paintRoundedTextBadge(graphics, new Point((int) mEg.mLeftCurve.x2, (int) mEg.mLeftCurve.y2), mDescription);
        }

        graphics.setColor(mColor);

        // draw head
        mEg.computeHead();

        // if selected draw interface control points
        if (mIsSelected) {
            graphics.setColor(Color.DARK_GRAY);
            graphics.setStroke(new BasicStroke(0.5f));
            graphics.drawLine((int) mEg.mCurve.x1, (int) mEg.mCurve.y1, (int) mEg.mCurve.ctrlx1,
                              (int) mEg.mCurve.ctrly1);
            graphics.drawLine((int) mEg.mCurve.x2, (int) mEg.mCurve.y2, (int) mEg.mCurve.ctrlx2,
                              (int) mEg.mCurve.ctrly2);
            graphics.setStroke(new BasicStroke(mPreferences.sNODEWIDTH / 30.0f, BasicStroke.CAP_BUTT,
                                               BasicStroke.JOIN_MITER));

            if (mCP1Selected) {
                graphics.setColor(mColor);
            }

            graphics.drawOval((int) mEg.mCurve.ctrlx1 - 7, (int) mEg.mCurve.ctrly1 - 7, 14, 14);
            graphics.fillOval((int) mEg.mCurve.ctrlx1 - 7, (int) mEg.mCurve.ctrly1 - 7, 14, 14);
            graphics.setColor(Color.DARK_GRAY);

            if (mCP2Selected) {
                graphics.setColor(mColor);
            }

            graphics.drawOval((int) mEg.mCurve.ctrlx2 - 7, (int) mEg.mCurve.ctrly2 - 7, 14, 14);
            graphics.fillOval((int) mEg.mCurve.ctrlx2 - 7, (int) mEg.mCurve.ctrly2 - 7, 14, 14);
            graphics.setColor(Color.DARK_GRAY);
            graphics.drawRect((int) mEg.mCurve.x1 - 7, (int) mEg.mCurve.y1 - 7, 14, 14);
            graphics.drawPolygon(mEg.mHead);
            graphics.fillRect((int) mEg.mCurve.x1 - 7, (int) mEg.mCurve.y1 - 7, 14, 14);
            graphics.fillPolygon(mEg.mHead);
        } else {
            graphics.setStroke(new BasicStroke(mPreferences.sNODEWIDTH / 30.0f, BasicStroke.CAP_BUTT,
                                               BasicStroke.JOIN_MITER));
            graphics.fillPolygon(mEg.mHead);
            graphics.setColor(mColor);
            graphics.drawPolygon(mEg.mHead);
        }

        if (hasAlternativeTargetNodes) {

            // String targets = mDataEdge.getStart();
            String targets = mDataEdge.getAltStartNodesAsString();

            // center the text
            mFontWidthCorrection = mFM.stringWidth(targets);

            // Get the current transform
            AffineTransform currentAT = graphics.getTransform();

            // Perform transformation
            AffineTransform at = new AffineTransform();

            graphics.translate(mEg.mAbsoluteEndPos.x, mEg.mAbsoluteEndPos.y);
            at.setToRotation((2 * Math.PI) - (mEg.mArrowDir + (Math.PI / 2)));
            graphics.transform(at);
            graphics.setColor(Color.WHITE);
            paintRoundedTextBadge(graphics, new Point(-(mFontWidthCorrection + 5), 0), targets);
            graphics.setTransform(currentAT);
        }

        // draw activity cue
        if (mVisualisationTask != null) {
            if (mVisualisationTask.getActivityTime() <= 20) {

                // fade out
                int gray = ((20 - mVisualisationTask.getActivityTime()) * 6);

                graphics.setColor(new Color(246 - gray, gray, gray, ((mPreferences.sACTIVITYTRACE)
                        ? 100
                        : 5 * mVisualisationTask.getActivityTime())));
            } else {

                // visualise activity
                graphics.setColor(new Color(246, 0, 0, 100));
            }

            graphics.setStroke(new BasicStroke(20f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER));
            graphics.draw(mEg.mCurve);
        }

//      }
//    }
    }

    public void drawArrow(Graphics2D g2d, int x, int y, float stroke) {    // int xCenter, int yCenter,
        g2d.setColor(mColor);

        double aDir = Math.atan2(x, y);    // xCenter-x,yCenter-y);

        g2d.setStroke(new BasicStroke(stroke));
        g2d.drawLine(x + 2, y + 2, 40, 40);
        g2d.setStroke(new BasicStroke(1f));    // make the arrow head solid even if dash pattern has been specified

        Polygon tmpPoly = new Polygon();
        int     i1      = 16;    // + (int) (stroke * 2);

        tmpPoly.addPoint(x, y);      // arrow tip
        tmpPoly.addPoint(x + xCor(i1, aDir + .5), y + yCor(i1, aDir + .5));
        tmpPoly.addPoint(x + xCor(i1, aDir - .5), y + yCor(i1, aDir - .5));
        tmpPoly.addPoint(x, y);      // arrow tip
        g2d.fillPolygon(tmpPoly);    // paint arrow head
    }

    /**
     * helper method used to provive an arrow for the edge
     *
     * @param len
     * @param dir
     * @return
     */
    private static int yCor(int len, double dir) {
        return (int) (len * Math.cos(dir));
    }

    /**
     * helper method used to provive an arrow for the edge
     *
     * @param len
     * @param dir
     * @return
     */
    private static int xCor(int len, double dir) {
        return (int) (len * Math.sin(dir));
    }

    /**
     * Nullifies the VisalisationTimer thread
     */
    public void stopVisualisation() {
        mVisualisationTimer.purge();
        mVisualisationTimer.cancel();
        mVisualisationTimer = null;
    }

    /*
     * Implements EventListener
     */
    public void update(EventObject event) {
        if (mPreferences.sVISUALISATION) {
            if (event instanceof EdgeExecutedEvent) {
                de.dfki.vsm.model.sceneflow.Edge edge = ((EdgeExecutedEvent) event).getEdge();

                if (edge.equals(mDataEdge)) {
                    if (mVisualisationTask != null) {
                        mVisualisationTask.cancel();
                    }

                    mVisualisationTask = new VisualisationTask(mPreferences.sVISUALISATIONTIME, this);
                    mVisualisationTimer.schedule(mVisualisationTask, 0, 15);
                }
            }
        }
    }
}

