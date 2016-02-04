package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.action.ModifyEdgeAction;
import de.dfki.vsm.editor.event.EdgeEditEvent;
import de.dfki.vsm.editor.event.EdgeExecutedEvent;
import de.dfki.vsm.editor.event.EdgeSelectedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.event.SceneStoppedEvent;
import de.dfki.vsm.editor.util.EdgeGraphics;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.LogicalCond;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import static de.dfki.vsm.Preferences.sCEDGE_COLOR;
import static de.dfki.vsm.Preferences.sEEDGE_COLOR;
import static de.dfki.vsm.Preferences.sFEDGE_COLOR;
import static de.dfki.vsm.Preferences.sIEDGE_COLOR;
import static de.dfki.vsm.Preferences.sPEDGE_COLOR;
import static de.dfki.vsm.Preferences.sTEDGE_COLOR;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * @author Patrick Gebhard
 * @author Not me
 */
public class Edge extends JComponent implements EventListener, Observer, MouseListener {

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
    private WorkSpacePanel   mWorkSpace                = null;

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
	private VisualisationTask mVisualisationTask = null;
	private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
	private final EventDispatcher mEventMulticaster = EventDispatcher.getInstance();

    // edit panel
    private JPanel    mTextPanel   = null;
    private JTextPane mValueEditor = null;
    private boolean   mEditMode    = false;
    SimpleAttributeSet attribs;

    //
    // other stuff
    private String             mName;
    private String             mDescription;
    private Color              mColor;
    private EditorConfig mEditorConfig;
    private Timer              mVisualisationTimer;

    public enum TYPE {
        EEDGE, TEDGE, CEDGE, PEDGE, IEDGE, FEDGE
    }

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

        initEditBox();
    }

    public Edge(WorkSpacePanel ws, de.dfki.vsm.model.sceneflow.Edge edge, TYPE type, Node sourceNode, Node targetNode) {
        mDataEdge           = edge;
        mWorkSpace          = ws;
        mEditorConfig       = mWorkSpace.getEditorConfig();
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
        initEditBox();
    }

    // TODO: Neuer Konstruktor, der Source und Target dockpoint "mitbekommt"
    public Edge(WorkSpacePanel ws, de.dfki.vsm.model.sceneflow.Edge edge, TYPE type, Node sourceNode, Node targetNode,
                Point sourceDockPoint, Point targetDockpoint) {
        mDataEdge           = edge;
        mWorkSpace          = ws;
        mEditorConfig        = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig();
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
        initEditBox();
    }

    @Override
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
        map.put(TextAttribute.SIZE, mEditorConfig.sWORKSPACEFONTSIZE);

        // Derive the font from the attribute map
        Font font = Font.getFont(map);

        // Derive the node's font metrics from the font
        mFM = getFontMetrics(font);

        // Set the edge's font to the updated font
        setFont(font);
        mFontWidthCorrection  = mFM.stringWidth(mName) / 2;
        mFontHeightCorrection = (mFM.getAscent() - mFM.getDescent()) / 2;
    }

    class MyDocumentListener implements DocumentListener {
         
        @Override
        // character added
        public void insertUpdate(DocumentEvent e) {            
            if(mType == TYPE.CEDGE){
                
                if(!validate(mValueEditor.getText())){
                   // wrong condition                   
                }
                else{
                    // correct condition
                }  
            }
        }
        
        @Override
        // character removed
        public void removeUpdate(DocumentEvent e) {
            if(mType == TYPE.CEDGE){
                
                if(!validate(mValueEditor.getText())){
                    // wrong condition
                }
                else{
                    // correct condition
                } 
            }
        }
        @Override
        public void changedUpdate(DocumentEvent e) {         
            //Plain text components do not fire these events
        }        
    }
    
    private boolean validate(String condition) {
                  
        String inputString = condition;

        try {
            _SFSLParser_.parseResultType = _SFSLParser_.LOG;
            _SFSLParser_.run(inputString);

            LogicalCond log = _SFSLParser_.logResult;

            return (log != null) &&!_SFSLParser_.errorFlag;
        } catch (Exception e) {
            
            return false;
        }
    }
    
    /* 
    * Initialize mTextPane and mValueEditor
    */
    private void initEditBox() {
        
        setLayout(null);
         
        mTextPanel = new JPanel();
        mTextPanel.setLayout(new BoxLayout(mTextPanel, BoxLayout.Y_AXIS));        
        mTextPanel.setBackground(Color.WHITE);
        
        Color borderColor = Preferences.sTEDGE_COLOR;
        
        switch (mType) {
          
            case TEDGE :
                borderColor = Preferences.sTEDGE_COLOR;
                break;

            case CEDGE :
                borderColor = Preferences.sCEDGE_COLOR;
                break;

            case PEDGE :
                borderColor = Preferences.sPEDGE_COLOR;
                break;

            case IEDGE :
                borderColor = Preferences.sIEDGE_COLOR;
                break;
            }
        
        mTextPanel.setBorder(BorderFactory.createLineBorder(borderColor));  
        
        mValueEditor = new JTextPane();
        mValueEditor.getDocument().addDocumentListener(new MyDocumentListener());
        
        attribs = new SimpleAttributeSet();
        StyleConstants.setAlignment(attribs, StyleConstants.ALIGN_CENTER);
        StyleConstants.setFontFamily(attribs, Font.SANS_SERIF);
        StyleConstants.setFontSize(attribs, 16);
        mValueEditor.setParagraphAttributes(attribs, true);
        
        mTextPanel.add(Box.createRigidArea(new Dimension(5, 5)));
        mTextPanel.add(mValueEditor);
        mTextPanel.add(Box.createRigidArea(new Dimension(5, 5)));
        
         
        if (mDataEdge != null) {
            if (mType.equals(TYPE.TEDGE)) {
                mValueEditor.setText("" + ((TEdge) mDataEdge).getTimeout());
            } else if (mType.equals(TYPE.PEDGE)) {
                mValueEditor.setText("" + ((PEdge) mDataEdge).getProbability());
            } else {
                mValueEditor.setText(mDescription);
            }
        }

        Action pressedAction = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setDeselected();

                NodeSelectedEvent evt = new NodeSelectedEvent(mWorkSpace,
                                            mWorkSpace.getSceneFlowManager().getCurrentActiveSuperNode());

                EventDispatcher.getInstance().convey(evt);
                updateFromTextEditor();
                
                 if(!validate(mValueEditor.getText())){
                    EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getFooterLabel().setForeground(Preferences.sIEDGE_COLOR);
                       EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().setMessageLabelText(
                        "Invalid Condition");
                    EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getFooterLabel().setForeground(Color.BLACK);
                    // wrong condition
                }
                else{
                    // correct condition
                } 
            }
        };
        
        Action escapeAction = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setDeselected();
            }
        };

        mValueEditor.getInputMap().put(KeyStroke.getKeyStroke("ENTER"), "enter");
        mValueEditor.getActionMap().put("enter", pressedAction);
        mValueEditor.getInputMap().put(KeyStroke.getKeyStroke("ESCAPE"), "escape");
        mValueEditor.getActionMap().put("escape", escapeAction);
    }
    
    /*
    *   Take input value of mValueEditor and set it as value of the edge
    */
    public void updateFromTextEditor() {
        String input = mValueEditor.getText();

        if (mType.equals(TYPE.TEDGE)) {
            try {
                ((TEdge) mDataEdge).setTimeout(Long.valueOf(input));
            } catch (NumberFormatException e) {
                mLogger.warning("Invalid Number Format");
            }
            
        } else if (mType.equals(TYPE.CEDGE)) {
            try {
                _SFSLParser_.parseResultType = _SFSLParser_.LOG;
                _SFSLParser_.run(input);

                LogicalCond log = _SFSLParser_.logResult;

                if ((log != null) &&!_SFSLParser_.errorFlag) {
                    ((CEdge) mDataEdge).setCondition(log);
                } else {
                    EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().setMessageLabelText(
                        "Remember to wrap condition in parenthesis");
                    // Do nothing
                }
            } catch (Exception e) {}
            
        } else if (mType.equals(TYPE.IEDGE)) {
            try {
                _SFSLParser_.parseResultType = _SFSLParser_.LOG;
                _SFSLParser_.run(input);

                LogicalCond log = _SFSLParser_.logResult;

                if ((log != null) &&!_SFSLParser_.errorFlag) {
                    ((IEdge) mDataEdge).setCondition(log);
                } else {}
            } catch (Exception e) {}
        }

        EditorInstance.getInstance().refresh();
    }

    

    public void setDeselected() {
        mIsSelected  = false;
        mCP1Selected = false;
        mCP2Selected = false;
        mCSPSelected = false;
        mCEPSelected = false;
        mEditMode    = false;
        remove(mTextPanel);
        repaint();
    }

    @Override
    public void mouseClicked(java.awt.event.MouseEvent event) {

        //////////!!!!!!!!!!!!!!!!!!!!
        // PG: 25.2.11 DISABELD: System.err.println("Sending node selected event");
        EventDispatcher.getInstance().convey(new EdgeSelectedEvent(this, this.getDataEdge()));
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
        // revalidate();
        repaint();

        // show contect menu
        if ((event.getButton() == MouseEvent.BUTTON3) && (event.getClickCount() == 1)) {
            mWorkSpace.showContextMenu(event, this);
        } 
        else if ((event.getClickCount() == 2)) {
            
            if (mType.equals(TYPE.TEDGE)) {
                String timeout = getDescription();
                timeout = timeout.replace("m", "");
                timeout = timeout.replace("s", "");
                timeout = timeout.replace(" ", "");
                timeout = timeout.replace("\n", "");
                mValueEditor.setText(timeout);
                
            } else if (mType.equals(TYPE.PEDGE)) {
                ModifyEdgeAction modifyAction = new ModifyEdgeAction(this, mWorkSpace);

                modifyAction.run();
                EditorInstance.getInstance().refresh();
                
                
            } else if (mType.equals(TYPE.CEDGE) || mType.equals(TYPE.IEDGE)) {
                mValueEditor.setText(this.getDescription());
            }

            if (mType.equals(TYPE.TEDGE) || mType.equals(TYPE.CEDGE) || mType.equals(TYPE.IEDGE)) {

                // mValueEditor.setText(getDescription());
             
                mValueEditor.requestFocus();
                mEditMode = true;
                EventDispatcher.getInstance().convey(new EdgeEditEvent(this, this.getDataEdge()));
                add(mTextPanel);
                
                mValueEditor.setText(mValueEditor.getText()); // hack to make mValueEditor visible
            }
        }
    }

    
    /*
     * Handles the mouse pressed event
     */
    @Override
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

    @Override
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

    @Override
    public void mouseEntered(java.awt.event.MouseEvent e) {}

    @Override
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
        graphics.setStroke(new BasicStroke(mEditorConfig.sNODEWIDTH / 30.0f, BasicStroke.CAP_BUTT,
                                           BasicStroke.JOIN_MITER));
        graphics.draw(mEg.mCurve);

        if (mEditMode == false) {
            if (mDescription.length() > 0) {
                paintRoundedTextBadge(graphics, new Point((int) mEg.mLeftCurve.x2, (int) mEg.mLeftCurve.y2),
                                      mDescription);
            }
        } else {
            
          //  Dimension size = mValueEditor.getPreferredSize();

            graphics.setColor(mColor);

            int x      = (int) mEg.mLeftCurve.x2 - (mValueEditor.getText().length() * 7);
            int y      = (int) mEg.mLeftCurve.y2 - 20;
            int width  = 20 + 10*mValueEditor.getText().length();
            int height = 40;

            mTextPanel.setBounds(x, y, width, height);
            mValueEditor.requestFocusInWindow();
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
            graphics.setStroke(new BasicStroke(mEditorConfig.sNODEWIDTH / 30.0f, BasicStroke.CAP_BUTT,
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
            graphics.setStroke(new BasicStroke(mEditorConfig.sNODEWIDTH / 30.0f, BasicStroke.CAP_BUTT,
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

                graphics.setColor(new Color(246 - gray, gray, gray, ((mEditorConfig.sACTIVITYTRACE)
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

    public boolean isInEditMode() {
        return mEditMode;
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
    @Override
    public void update(EventObject event) {
        if (event instanceof SceneStoppedEvent) {
            mVisualisationTask = null;
            repaint();
        }

        if (mEditorConfig.sVISUALISATION) {
            if (event instanceof EdgeExecutedEvent) {
                de.dfki.vsm.model.sceneflow.Edge edge = ((EdgeExecutedEvent) event).getEdge();

                if (edge.equals(mDataEdge)) {
                    if (mVisualisationTask != null) {
                        mVisualisationTask.cancel();
                    }

                    mVisualisationTask = new VisualisationTask(mEditorConfig.sVISUALISATIONTIME, this);
                    mVisualisationTimer.schedule(mVisualisationTask, 0, 15);
                }
            }
        }
    }
}
