package de.dfki.vsm.editor;

import de.dfki.vsm.editor.script.ScriptEditorPanel;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
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
 * *************************************************************************
 *
 *
 *
 ***************************************************************************
 */
public class ProjectToolBar extends JPanel implements Observer, EventListener, ActionListener {
    
    private final ToolBarItem mSuperNodeEntry = new ToolBarItem("Super Node", "SUPERNODE_ENTRY", Node.Type.SuperNode);
    private final ToolBarItem mBasicNodeEntry = new ToolBarItem("Basic Node", "BASICNODE_ENTRY", Node.Type.BasicNode);
    private final ToolBarItem mEEdgeEntry = new ToolBarItem("Epsilon Edge", "EEDGE_ENTRY", new Edge(Edge.TYPE.EEDGE));
    private final ToolBarItem mTEdgeEntry = new ToolBarItem("Timeout Edge", "TEDGE_ENTRY", new Edge(Edge.TYPE.TEDGE));
    private final ToolBarItem mPEdgeEntry = new ToolBarItem("Probability Edge", "PEDGE_ENTRY", new Edge(Edge.TYPE.PEDGE));
    private final ToolBarItem mCEdgeEntry = new ToolBarItem("Conditional Edge", "CEDGE_ENTRY", new Edge(Edge.TYPE.CEDGE));
    private final ToolBarItem mIEdgeEntry = new ToolBarItem("Interruptive Edge", "IEDGE_ENTRY", new Edge(Edge.TYPE.IEDGE));
    private final ToolBarItem mFEdgeEntry = new ToolBarItem("Fork Edge", "FEDGE_ENTRY", new Edge(Edge.TYPE.FEDGE));
    private final ToolBarItem mCommentEntry = new ToolBarItem("Comment", "COMMENT_ENTRY", new Comment());
    
    private final SceneFlow mSceneFlow;
    private final ScriptEditorPanel mScriptEditorPanel;
  
    // Drag & Drop support
    private DragSource mDragSource;
    private DragGestureListener mDragGestureListener;
    private DragSourceListener mDragSourceListener;
    private int mAcceptableDnDActions;
    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventCaster = EventCaster.getInstance();
    

    private final ProjectData   mProject;
    private final DialogActInterface mDialogAct;
    @Override
    public void update(java.util.Observable obs, Object obj) {
        updateUI();
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    @Override
    public void update(EventObject event) {

        System.err.println("EventListener of ElementTree");
        // Update the visual appearance of the ElementTree
        updateUI();
        throw new Error();
    }
    
    

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    public ProjectToolBar(SceneFlow sceneFlow, ProjectData project, ScriptEditorPanel scriptEditor) {
        //super(new DefaultTreeModel(null));
        //
        setLayout(new GridLayout(0, 2));
        mSceneFlow = sceneFlow;
        mScriptEditorPanel = scriptEditor;
        mProject = project;
        mDialogAct = mProject.getDialogAct();
        setBorder(BorderFactory.createEmptyBorder());
        //setCellRenderer(new CellRenderer());
        setBackground(Color.WHITE);
        //setRootVisible(false);
        // setRowHeight(0);
        //
//        initDnDSupport();
        initComponents();
        //addMouseListener(getMouseAdapter(this));
        ToolTipManager.sharedInstance().registerComponent(this);
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    private void initComponents() {
        // Add basic element entries
        add(mSuperNodeEntry);
        add(mBasicNodeEntry);
        add(mEEdgeEntry);
        add(mTEdgeEntry);
        add(mPEdgeEntry);
        add(mCEdgeEntry);
        add(mIEdgeEntry);
        add(mFEdgeEntry);
        add(mCommentEntry);
        //initDnDSupport();
    }
    private void initDnDSupport() {
        // Create the default drag source
        mDragSource = DragSource.getDefaultDragSource();
        // Install the drag source listener
        mDragSourceListener = new DragSourceListener() {

            @Override
            public void dragEnter(DragSourceDragEvent dsde) {
            }

            @Override
            public void dragOver(DragSourceDragEvent dsde) {
            }

            @Override
            public void dropActionChanged(DragSourceDragEvent dsde) {
            }

            @Override
            public void dragExit(DragSourceEvent dse) {
            }

            @Override
            public void dragDropEnd(DragSourceDropEvent dsde) {
            }
        };
        // Install the drag gesture listener
        mDragGestureListener = new DragGestureListener() {

            @Override
            public void dragGestureRecognized(DragGestureEvent event) {
                // TODO: NULLPOINTEREXCEPTION abfangen
                ToolBarItem selectedEntry = (ToolBarItem)event.getComponent();
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
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    
}
