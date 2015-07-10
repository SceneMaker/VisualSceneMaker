package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.ToolBarItem;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.ToolTipManager;

/**
 *
 *
 *
 *
 *
 */
public class ProjectToolPanel extends JPanel implements Observer, EventListener, ActionListener {
    private final ToolBarItem mSuperNodeEntry = new ToolBarItem("Super Node", "Holds Sub-Scences flow",
                                                    "SUPERNODE_ENTRY", Node.Type.SuperNode);
    private final ToolBarItem mBasicNodeEntry = new ToolBarItem("Basic Node", "Holds Scenes Actions",
                                                    "BASICNODE_ENTRY", Node.Type.BasicNode);
    private final ToolBarItem mEEdgeEntry = new ToolBarItem("Epsilon Edge", "Creates Epsilon Transition",
                                                "EEDGE_ENTRY", new Edge(Edge.TYPE.EEDGE));
    private final ToolBarItem mTEdgeEntry = new ToolBarItem("Timeout Edge", "Creates Timeout Transition",
                                                "TEDGE_ENTRY", new Edge(Edge.TYPE.TEDGE));
    private final ToolBarItem mPEdgeEntry = new ToolBarItem("Probability Edge", "Creates Probability Transition",
                                                "PEDGE_ENTRY", new Edge(Edge.TYPE.PEDGE));
    private final ToolBarItem mCEdgeEntry = new ToolBarItem("Conditional Edge", "Creates Conditional Transition",
                                                "CEDGE_ENTRY", new Edge(Edge.TYPE.CEDGE));
    private final ToolBarItem mIEdgeEntry = new ToolBarItem("Interruptive Edge", "Creates Interrutive Transition",
                                                "IEDGE_ENTRY", new Edge(Edge.TYPE.IEDGE));
    private final ToolBarItem mFEdgeEntry = new ToolBarItem("Fork Edge", "Creates Forked Transition", "FEDGE_ENTRY",
                                                new Edge(Edge.TYPE.FEDGE));
    private final ToolBarItem mCommentEntry = new ToolBarItem("Comment", "Adds a Comment", "COMMENT_ENTRY",
                                                  new Comment());

    //
    private final LOGDefaultLogger mLogger      = LOGDefaultLogger.getInstance();
    private final EventDispatcher      mEventCaster = EventDispatcher.getInstance();

    // Drag & Drop support
    private DragSource          mDragSource;
    private DragGestureListener mDragGestureListener;
    private DragSourceListener  mDragSourceListener;
    private int                 mAcceptableDnDActions;

    /**
     *
     *
     *
     *
     *
     */
    public ProjectToolPanel() {

        // super(new DefaultTreeModel(null));
        //
        setLayout(new GridLayout(0, 2));
        setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.DARK_GRAY));

        // setCellRenderer(new CellRenderer());
        setBackground(Color.WHITE);
        setPreferredSize(new Dimension(250, 330));
        setMinimumSize(new Dimension(250, 330));
        setMaximumSize(new Dimension(250, 330));

        // setRootVisible(false);
        // setRowHeight(0);
        //
//      initDnDSupport();
        initComponents();

        // addMouseListener(getMouseAdapter(this));
        ToolTipManager.sharedInstance().registerComponent(this);
    }

    @Override
    public void update(java.util.Observable obs, Object obj) {
        updateUI();
    }

    /**
     *
     *
     *
     *
     *
     */
    @Override
    public void update(EventObject event) {
        System.err.println("EventListener of ElementTree");

        // Update the visual appearance of the ElementTree
        updateUI();

        throw new Error();
    }

    /**
     *
     *
     *
     *
     *
     */
    private void initComponents() {

        // Add basic element entries
        add(mSuperNodeEntry);
        add(mBasicNodeEntry);
        add(mEEdgeEntry);
        add(mPEdgeEntry);
        add(mFEdgeEntry);
        add(mCEdgeEntry);
        add(mTEdgeEntry);
        add(mIEdgeEntry);
        add(mCommentEntry);

        // initDnDSupport();
    }

    private void initDnDSupport() {

        // Create the default drag source
        mDragSource = DragSource.getDefaultDragSource();

        // Install the drag source listener
        mDragSourceListener = new DragSourceListener() {
            @Override
            public void dragEnter(DragSourceDragEvent dsde) {}
            @Override
            public void dragOver(DragSourceDragEvent dsde) {}
            @Override
            public void dropActionChanged(DragSourceDragEvent dsde) {}
            @Override
            public void dragExit(DragSourceEvent dse) {}
            @Override
            public void dragDropEnd(DragSourceDropEvent dsde) {}
        };

        // Install the drag gesture listener
        mDragGestureListener = new DragGestureListener() {
            @Override
            public void dragGestureRecognized(DragGestureEvent event) {

                // TODO: NULLPOINTEREXCEPTION abfangen
                ToolBarItem selectedEntry = (ToolBarItem) event.getComponent();

                mDragSource.startDrag(event, DragSource.DefaultCopyDrop, selectedEntry, mDragSourceListener);
            }
        };

        // Set the acceptable actions
        mAcceptableDnDActions = DnDConstants.ACTION_COPY;

        // Set the default drag gesture recognizer
        mDragSource.createDefaultDragGestureRecognizer(this, mAcceptableDnDActions, mDragGestureListener);
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        throw new UnsupportedOperationException("Not supported yet.");    // To change body of generated methods, choose Tools | Templates.
    }
}
