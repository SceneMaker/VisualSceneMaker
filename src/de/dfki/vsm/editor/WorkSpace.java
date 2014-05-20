package de.dfki.vsm.editor;

import de.dfki.vsm.editor.action.ChangeNodeTypeAction;
import de.dfki.vsm.editor.action.CopyNodesAction;
import de.dfki.vsm.editor.action.CreateCommentAction;
import de.dfki.vsm.editor.action.CreateEdgeAction;
import de.dfki.vsm.editor.action.CreateNodeAction;
import de.dfki.vsm.editor.action.CutNodesAction;
import de.dfki.vsm.editor.action.DeflectEdgeAction;
import de.dfki.vsm.editor.action.ModifyEdgeAction;
import de.dfki.vsm.editor.action.NormalizeEdgeAction;
import de.dfki.vsm.editor.action.PasteNodesAction;
import de.dfki.vsm.editor.action.RemoveCommentAction;
import de.dfki.vsm.editor.action.RemoveEdgeAction;
import de.dfki.vsm.editor.action.RemoveNodeAction;
import de.dfki.vsm.editor.action.RemoveNodesAction;
import de.dfki.vsm.editor.action.StraightenEdgeAction;
import de.dfki.vsm.editor.action.ToggleStartNodeAction;
import de.dfki.vsm.editor.dialog.CmdDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.util.GridManager;
import static de.dfki.vsm.editor.util.Preferences.sCEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sFEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sGRID_NODEWIDTH;
import static de.dfki.vsm.editor.util.Preferences.sIEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sNODEHEIGHT;
import static de.dfki.vsm.editor.util.Preferences.sPEDGE_COLOR;
import static de.dfki.vsm.editor.util.Preferences.sTEDGE_COLOR;
import de.dfki.vsm.editor.util.SceneFlowLayoutManager;
import de.dfki.vsm.editor.util.SceneFlowManager;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.EEdge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.PlaySceneGroup;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.UsrCmd;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetAdapter;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Observer;
import java.util.Set;
import java.util.Vector;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.InputMap;
import javax.swing.JComponent; 
import javax.swing.JMenuItem; 
import javax.swing.JOptionPane; 
import javax.swing.JPanel; 
import javax.swing.JPopupMenu; 
import javax.swing.JSeparator; 
import javax.swing.KeyStroke; 

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class WorkSpace extends JPanel implements Observer, EventListener, MouseListener, MouseMotionListener {

  //
    // The clipboard
    private final ClipBoard mClipboard = new ClipBoard();
    // Drag & Drop support
    private DropTarget mDropTarget;
    private DropTargetListener mDropTargetListener;
    private int mAcceptableActions;
    // The parent SceneFlowEditor (TODO: remove)
    private final SceneFlowEditor mSceneFlowEditor;
    // Elements to draw
    private final Set<Node> mNodeSet = new HashSet<Node>();
    private final Set<Edge> mEdgeSet = new HashSet<Edge>();
    private final Set<Comment> mCmtSet = new HashSet<Comment>();
    private final HashMap<Node, CmdBadge> mCmdBadgeMap = new HashMap<Node, CmdBadge>();
    // Variable display
    private VarBadge mVarDisplay = null;
    private NodeVariableBadge mNodeVariableDisplay = null;
    // Flags for mouse interaction
    private Node mSelectedNode = null;
    public Edge mSelectedEdge = null;
    private Comment mSelectedComment = null;
    private VarBadge mSelectedVariableBadge = null;
    private Rectangle2D.Double mAreaSelection = null;
    private Rectangle2D.Double mDrawArea = null;
    private Point mLastMousePosition = new Point(0, 0);
    private boolean mDoAreaSelection = false;
    private boolean mDoAreaAction = false;
    private Set<Node> mSelectedNodes = new HashSet<Node>();
    // Variables for edge creation
    private Edge mEdgeInProgress = null;
    private Node mEdgeSourceNode = null;
    private Point mSelectNodePoint = null;
    private final AttributedString sEdgeCreationHint = new AttributedString("Select Target Node");
    //
    private boolean mIgnoreMouseInput = false;
    private boolean mSelectTargetNodeMode = false;
    private boolean mEdgeSourceNodeReassign = false;
    private boolean mEdgeTargetNodeReassign = false;
    private Node mReassignNode = null;
    // Snap to grid support
    public GridManager mGridManager = null;
    //
    public final Observable mObservable = new Observable();
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventCaster = EventCaster.getInstance();
    //
    private final LinkedList<VarBadge> mVarBadgeStack = new LinkedList<VarBadge>();

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public class Observable extends java.util.Observable {

        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    public void update(java.util.Observable obs, Object obj) {
        //mLogger.message("WorkSpace.update(" + obj + ")");
        mObservable.update(obj);
        // rebuild node position
        mGridManager.update();
        for (Node node : mNodeSet) {
          // node.resetLocation(mGridManager.getNodeLocation(node.getLocation()));
        }
        revalidate();
        repaint();
    }

    /**
     * *************************************************************************
     *
     *
     *************************************************************************
     */
    public void update(EventObject event) {
    
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public class ClipBoard extends HashSet<de.dfki.vsm.model.sceneflow.Node> {
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public WorkSpace(SceneFlowEditor sceneFlowEditor) {
        
        mSceneFlowEditor = sceneFlowEditor;
        mGridManager = new GridManager(this);
        // Add the mouse listeners
        addMouseMotionListener(this);
        addMouseListener(this);
        
        setKeyBindings(); 
        
        // Init the drag & drop support
        initDnDSupport();
        // init layout
        setLayout(new SceneFlowLayoutManager());
        setBorder(BorderFactory.createEmptyBorder());
        mDrawArea = new Rectangle2D.Double();
        // init selection
        mAreaSelection = new Rectangle2D.Double();
        // TODO: move to somewhere??!!
        NodeSelectedEvent e = new NodeSelectedEvent(this, getSceneFlowManager().getCurrentActiveSuperNode());

        mEventCaster.convey(e);
        // display components
        showVariableBadge();
        showNodesOnWorkSpace();
        showEdgesOnWorkSpace();
    }

    public void clearClipBoard() {
        mClipboard.clear();
    }

    public ClipBoard getClipBoard() {
        return mClipboard;
    }

    public SceneFlowManager getSceneFlowManager() {
        return mSceneFlowEditor.getSceneFlowManager();
    }

    public SceneFlowEditor getSceneFlowEditor() {
        return mSceneFlowEditor;
    }

    public GridManager getGridManager() {
        return mGridManager;
    }

    public Node getNode(String id) {
        for (Node node : mNodeSet) {
            if (node.getDataNode().getId().equals(id)) {
                return node;
            }
        }
        return null;
    }
    
    public Set<Node> getNodes() {
        return mNodeSet;
    }

    public CmdBadge getCmdBadge(Node id) {
        return mCmdBadgeMap.get(id);
    }
    
    public Set<Edge> getEdges() {
        return mEdgeSet;
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    private void initDnDSupport() {
        mAcceptableActions = DnDConstants.ACTION_COPY;
        mDropTargetListener = new DropTargetAdapter() {

            @Override
            public void dragEnter(DropTargetDragEvent dtde) {
            }

            @Override
            public void dragOver(DropTargetDragEvent dtde) {
                Object data = null;
                DataFlavor flavor = null;
                try {
                    try {
                        flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
                    } catch (ClassNotFoundException e) {
                        e.printStackTrace();
                    }
                    data = dtde.getTransferable().getTransferData(flavor);
                } catch (java.awt.datatransfer.UnsupportedFlavorException e) {
                    e.printStackTrace();
                } catch (java.io.IOException e) {
                    e.printStackTrace();
                }

                if (data instanceof SceneGroup) {
                    Point pos = dtde.getLocation();
                    for (Node node : mNodeSet) {
                        if (node.containsPoint(pos.x, pos.y)) {
                            dtde.acceptDrag(dtde.getDropAction());
                            //System.err.println("Accept Drag over");
                            mSceneFlowEditor.setMessageLabelText("Scene allowed here");
                            return;
                        } else {
                            //System.err.println("Reject Drag over");
                            mSceneFlowEditor.setMessageLabelText("");
                        }
                    }
                    mSceneFlowEditor.setMessageLabelText("Drop scene on node");
                    dtde.rejectDrag();
                }

                if (data instanceof Comment) {
                    dtde.acceptDrag(dtde.getDropAction());
                    //System.err.println("Accept Drag over");
                }

                if (data instanceof Edge) {
                    Point pos = dtde.getLocation();
                    for (Node node : mNodeSet) {
                        if (node.containsPoint(pos.x, pos.y)) {
                            mSceneFlowEditor.setMessageLabelText("");
                            if (node.isEdgeAllowed(((Edge) data).getType())) {
                                dtde.acceptDrag(dtde.getDropAction());
                            } else {
                                mSceneFlowEditor.setMessageLabelText("Edge is not allowed at this node");
                            }
                            return;
                        } else {
                            mSceneFlowEditor.setMessageLabelText("");
                        }
                    }
                    mSceneFlowEditor.setMessageLabelText("Drag edge on a node to select edge source");
                    dtde.rejectDrag();
                }
            }

            @Override
            public void dragExit(DropTargetEvent dte) {
                mSceneFlowEditor.setMessageLabelText("");
            }

            public void drop(DropTargetDropEvent dtde) {
                mSceneFlowEditor.setMessageLabelText("");

                try {
                    // Get the data of the transferable
                    Object data = dtde.getTransferable().getTransferData(
                            new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType));
                    //
                    if (data instanceof Node.Type) {
                        //
                        createNode(dtde.getLocation(), (Node.Type) data);
            //
//                        revalidate();
//                        repaint();
                        //
                        dtde.acceptDrop(mAcceptableActions);
                        dtde.getDropTargetContext().dropComplete(true);
                    } else if (data instanceof Edge) {
                        createNewEdgeSelectSourceNode(
                                (Edge) data,
                                dtde.getLocation().x,
                                dtde.getLocation().y);
            //
//                        revalidate();
//                        repaint();
                        //
                        dtde.acceptDrop(mAcceptableActions);
                        dtde.getDropTargetContext().dropComplete(true);
                    } else if (data instanceof Comment) {
                        createComment(dtde.getLocation());
            //
//                        revalidate();
//                        repaint();
                        //
                        dtde.acceptDrop(mAcceptableActions);
                        dtde.getDropTargetContext().dropComplete(true);
                    } else if (data instanceof SceneGroup) {
                        for (Node node : mNodeSet) {
                            if (node.containsPoint(dtde.getLocation().x, dtde.getLocation().y)) {
                                createPSG(node, ((SceneGroup) data).getName());
                                //
                                dtde.acceptDrop(mAcceptableActions);
                                dtde.getDropTargetContext().dropComplete(true);
                //
                                //
//                                c.update();
                            } else {
                                mSceneFlowEditor.setMessageLabelText("");
                            }
                        }
                        // TODO: reject drop if not on a c!!!
                    } else if (data instanceof FunDef) {
                        for (Node node : mNodeSet) {
                            if (node.containsPoint(dtde.getLocation().x, dtde.getLocation().y)) {
                                createFunCall(node, ((FunDef) data).getName());
                                //
                                dtde.acceptDrop(mAcceptableActions);
                                dtde.getDropTargetContext().dropComplete(true);
                                //
//                                c.update();
                            } else {
                                mSceneFlowEditor.setMessageLabelText("");
                            }
                        }
                        // TODO: reject drop if not on a c!!!
                    } else {
                        dtde.rejectDrop();
                    }
                } catch (ClassNotFoundException e) {
                    dtde.rejectDrop();
                } catch (UnsupportedFlavorException e) {
                    dtde.rejectDrop();
                } catch (IOException e) {
                    dtde.rejectDrop();
                }
                // Update whole editor after a drop!!!!
                Editor.getInstance().update();
            }
        };
        mDropTarget = new DropTarget(this, mDropTargetListener);
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void showVariableBadge() {

        mVarDisplay = new VarBadge(getSceneFlowManager().getCurrentActiveSuperNode());
        add(mVarDisplay);
        mEventCaster.append(mVarDisplay);
        mObservable.addObserver(mVarDisplay);

    ////////
        // add/remove to/from stack
        //mVarBadgeStack.a(mVarDisplay);
        ////////
//        repaint();
    }

    public void showNodeVariables(Node node) {
        ArrayList<String> localTypeDefList = new ArrayList<String>();
        ArrayList<String> globalTypeDefList = new ArrayList<String>();
        ArrayList<String> localVarDefList = new ArrayList<String>();
        ArrayList<String> globalVarDefList = new ArrayList<String>();
        //
        Vector<TypeDef> typeDefs = node.getDataNode().getTypeDefList();
        for (TypeDef typeDef : typeDefs) {
            localTypeDefList.add(typeDef.getFormattedSyntax());
        }
        //
        Set<SuperNode> parentSNss = null;
        if (!getSceneFlowManager().isRootSuperNode(node.getDataNode())) {
            parentSNss = getSceneFlowManager().getParentSuperNodeSet(node.getDataNode());
        }
        if (parentSNss != null) {
            for (SuperNode sn : parentSNss) {
                Vector<TypeDef> snTypeDefs = sn.getTypeDefList();
                if (snTypeDefs.size() > 0) {
                    for (TypeDef typeDef : snTypeDefs) {
                        globalTypeDefList.add(typeDef.getFormattedSyntax() + " (" + sn.getName() + ") ");
                    }
                }
            }
        }
        //
        Vector<VarDef> varDefs = node.getDataNode().getVarDefList();
        for (VarDef varDef : varDefs) {
            localVarDefList.add(varDef.getFormattedSyntax());
        }
        //
        Set<SuperNode> parentSNs = null;
        if (!getSceneFlowManager().isRootSuperNode(node.getDataNode())) {
            parentSNs = getSceneFlowManager().getParentSuperNodeSet(node.getDataNode());
        }
        if (parentSNs != null) {
            for (SuperNode sn : parentSNs) {
                Vector<VarDef> snVarDefs = sn.getVarDefList();
                if (snVarDefs.size() > 0) {
                    for (VarDef varDef : snVarDefs) {
                        globalVarDefList.add(varDef.getFormattedSyntax() + " (" + sn.getName() + ") ");
                    }
                }
            }
        }
        //
        mNodeVariableDisplay = new NodeVariableBadge(node, this, localVarDefList, globalVarDefList, localTypeDefList, globalTypeDefList);
        add(mNodeVariableDisplay, 0);
    }

    private void selectNodesInArea() {
        mSelectedNodes = new HashSet<Node>();
        for (Node node : mNodeSet) {
            // add node only if it is not a history node
            if (node.getBounds().intersects(mDrawArea)) {
                if (node.getDataNode().isHistoryNode()) {
                    mSceneFlowEditor.setMessageLabelText("Copy, cut and remove actions are not allowed on History nodes!");
                }
                node.mSelected = true;
                mSelectedNodes.add(node);
            } else {
                node.mSelected = false;
            }
        }
    }

    public void deselectAllNodes() {
        mSelectedNodes = new HashSet<Node>();
        for (Node node : mNodeSet) {
            node.mSelected = false;
        }
        repaint();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void createNode(Point point, Node.Type type) {
       Point correctedPoint = new Point (point.x - sGRID_NODEWIDTH / 2, point.y - sGRID_NODEWIDTH / 2);
        new CreateNodeAction(this, mGridManager.getNodeLocation(correctedPoint), type).run();
    }

    private void createComment(Point point) {
        new CreateCommentAction(this, point).run();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void createPSG(Node node, String name) {
        PlaySceneGroup psgCmd = new PlaySceneGroup();
        psgCmd.setArg(new de.dfki.vsm.model.sceneflow.command.expression.condition.constant.String(name));
        node.getDataNode().addCmd(psgCmd);
    }

    public void createFunCall(Node node, String name) {
     
        UsrCmd cmd = new UsrCmd();
        
        cmd.setName(name);
        node.getDataNode().addCmd(cmd);
       
        Command newCmd = new CmdDialog(cmd).run();
        if (newCmd != null) {
            node.getDataNode().setCmdAt(newCmd, 0);

        }
    }

    /**
     * *************************************************************************
     *
     * Edge creation
     *
     *************************************************************************
     */
    public void createNewEdgeSelectSourceNode(Edge edge, int x, int y) {
    // Try to find the c on which the edge was dropped. If we do not find
        // such a c then the edge was dropped on the drawing area of the workspace
        // and we exit the method without creating a new edge.
        Node sourceNode = null;
        for (Node node : mNodeSet) {
            if (node.containsPoint(x, y)) {
                sourceNode = node;
                break;
            }
        }
        if (sourceNode == null) {
            return;
        }
    // Check if the type of this edge is allowed to be connected to the
        // source c. If the edge is not allowed then we exit the method.
        if (!sourceNode.isEdgeAllowed(edge.getType())) {
            return;
        }
        mSelectNodePoint = new Point(x, y);
    // Set the current edge in process, the current source
        // c and enter the target c selection mode.
        mEdgeInProgress = edge;
        mEdgeSourceNode = sourceNode;
        mSelectTargetNodeMode = true;
    //repaint();
        //
        mSceneFlowEditor.setMessageLabelText("Select target node or click on workspace to abort");
    }

    public void createNewEdgeSelectTargetNode(int x, int y) {
        mSelectTargetNodeMode = false;
        //repaint();
        mSceneFlowEditor.setMessageLabelText("");
    // Try to find the c on which the mouse was clicked. If we do not find
        // such a c then the mouse was clicked on the drawing area of the workspace
        // and we exit the method without creating a new edge.
        Node targetNode = null;
        for (Node n : mNodeSet) {
            if (n.containsPoint(x, y)) {
                targetNode = n;
                break;
            }
        }
        if (targetNode == null) {
            return;
        }
        // If we found a target c, then we create a new edge
        new CreateEdgeAction(this, mEdgeSourceNode, targetNode, mEdgeInProgress.getType()).run();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void cleanup() {
        // TODO: proper cleanup
        clearClipBoard();
        clear();
    }

    public void clear() {

        // Reset mouse interaction
        mIgnoreMouseInput = true;
        //  synchronized (ActivityEventMulticaster.getInstance().mActivityEventListenerListLock) {
        removeEventListeners();
        mObservable.deleteObservers();
    //  }
        // Clear the list of currently shown nodes and edges and
        // remove all components from the workspace. Additionally
        // clear the selected edges and nodes of the workspace.
        mNodeSet.clear();
        mEdgeSet.clear();
        mCmtSet.clear();
        mCmdBadgeMap.clear();
        removeAll();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void add(Comment c) {
        mCmtSet.add(c);
        super.add(c);
        mEventCaster.append(c);
    }

    public void remove(Comment c) {
        mCmtSet.remove(c);
        super.remove(c);
        mEventCaster.remove(c);
    }

    public void addNode(Node node) {
        mNodeSet.add(node);
        super.add(node);
        mEventCaster.append(node);
        mObservable.addObserver(node);
    }

    public void remove(Node node) {
        //  TODO: deselect all components instead
        if (mSelectedNode != null) {
            mSelectedNode = (mSelectedNode.equals(node)) ? null : mSelectedNode;
        }
        super.remove(node);
        mNodeSet.remove(node);
        mEventCaster.remove(node);
        mObservable.deleteObserver(node);
    }

    public void add(Edge edge) {
        super.add(edge);
        mEdgeSet.add(edge);
        mEventCaster.append(edge);
        mObservable.addObserver(edge);
    }

    public void remove(Edge edge) {
        //  TODO: deselect all components instead
        if (mSelectedEdge != null) {
            mSelectedEdge = (mSelectedEdge.equals(edge)) ? null : mSelectedEdge;
        }
        mEdgeSet.remove(edge);
        super.remove(edge);
        mEventCaster.remove(edge);
        mObservable.deleteObserver(edge);
    }

    public void addCmdBadge(Node node, CmdBadge badge) {
        super.add(badge);
        mCmdBadgeMap.put(node, badge);
        mEventCaster.append(badge);
        mObservable.addObserver(badge);
    }

    public void removeCmdBadge(Node node) {
        CmdBadge badge = mCmdBadgeMap.remove(node);
        super.remove(badge);
        mEventCaster.remove(badge);
        mObservable.deleteObserver(badge);
    }

    private void removeEventListeners() {
        if (mVarDisplay != null) {
            mEventCaster.remove(mVarDisplay);
        }
        for (CmdBadge c : mCmdBadgeMap.values()) {
            mEventCaster.remove(c);
            c.stopVisualisation();
        }
        for (Node n : mNodeSet) {
            mEventCaster.remove(n);
            n.stopVisualisation();
        }
        for (Edge e : mEdgeSet) {
            mEventCaster.remove(e);
            e.stopVisualisation();
        }
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void straightenAllEdges() {
        for (Edge edge : mEdgeSet) {
            edge.straightenEdge();
        }
        repaint();
    }

    public void normalizeAllEdges() {
        for (Edge edge : mEdgeSet) {
            edge.rebuildEdgeNicely();
        }
        repaint();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void showContextMenu(MouseEvent evt, Edge edge) {
        JPopupMenu pop = new JPopupMenu();
        JMenuItem item = new JMenuItem("Modify");
        ModifyEdgeAction modifyAction = new ModifyEdgeAction(edge, this);
        item.addActionListener(modifyAction.getActionListener());
        pop.add(item);
        item = new JMenuItem("Delete");
        RemoveEdgeAction deleteAction = new RemoveEdgeAction(this, edge);
        item.addActionListener(deleteAction.getActionListener());
        pop.add(item);
        item = new JMenuItem("Straighten");
        StraightenEdgeAction renameAction = new StraightenEdgeAction(this, edge);
        item.addActionListener(renameAction.getActionListener());
        pop.add(item);
        item = new JMenuItem("Normalize");
        NormalizeEdgeAction normalizeAction = new NormalizeEdgeAction(this, edge);
        item.addActionListener(normalizeAction.getActionListener());
        pop.add(item);
        pop.show(this, evt.getX(), evt.getY());
    }

    public void showContextMenu(MouseEvent evt, Comment comment) {
        JPopupMenu pop = new JPopupMenu();
        JMenuItem item = new JMenuItem("Delete");
        RemoveCommentAction deleteAction = new RemoveCommentAction(this, comment);
        item.addActionListener(deleteAction.getActionListener());
        pop.add(item);
        pop.show(this, comment.getX() + comment.getWidth(), comment.getY());
    }

    public void showContextMenu(MouseEvent evt, Node node) {
        JPopupMenu pop = new JPopupMenu();
        JMenuItem item = null;
        if (!node.getDataNode().isHistoryNode()) {
            HashMap<String, de.dfki.vsm.model.sceneflow.Node> startNodes = node.getDataNode().getParentNode().getStartNodeMap();
            item = new JMenuItem((startNodes.containsKey(node.getDataNode().getId())) ? "Unset Start" : "Set Start");
            ToggleStartNodeAction toggleStartnodeAction = new ToggleStartNodeAction(node, this);
            item.addActionListener(toggleStartnodeAction.getActionListener());
            pop.add(item);
            pop.add(new JSeparator());
            if (!(node.getDataNode() instanceof de.dfki.vsm.model.sceneflow.SuperNode)) {
                item = new JMenuItem("To Supernode");
                ChangeNodeTypeAction changetypeAction = new ChangeNodeTypeAction(this, node);
                item.addActionListener(changetypeAction.getActionListener());
                pop.add(item);
                pop.add(new JSeparator());
            }

            item = new JMenuItem("Copy");
            CopyNodesAction copyAction = new CopyNodesAction(this, node);
            item.addActionListener(copyAction.getActionListener());
            pop.add(item);
            item = new JMenuItem("Cut");
            CutNodesAction cutAction = new CutNodesAction(this, node);
            item.addActionListener(cutAction.getActionListener());
            pop.add(item);
            pop.add(new JSeparator());
        }

        if (!node.getDataNode().isHistoryNode()) {
            item = new JMenuItem("Delete");
            RemoveNodeAction deleteAction = new RemoveNodeAction(this, node);
            item.addActionListener(deleteAction.getActionListener());
            pop.add(item);
        }

        pop.show(this, node.getX() + node.getWidth(), node.getY());
    }

    public void multipleNodesContextMenu(MouseEvent evt, Node node) {
        // remove History node for actions
        HashSet<Node> filteredSelectedNodes = new HashSet<Node>();
        for (Node n : mSelectedNodes) {
            if (!n.getDataNode().isHistoryNode()) {
                filteredSelectedNodes.add(n);
            }
        }
        JPopupMenu pop = new JPopupMenu();
        JMenuItem item = new JMenuItem("Copy Nodes");
        CopyNodesAction copyAction = new CopyNodesAction(this, filteredSelectedNodes);
        item.addActionListener(copyAction.getActionListener());
        pop.add(item);
        item = new JMenuItem("Cut Nodes");
        CutNodesAction cutAction = new CutNodesAction(this, filteredSelectedNodes);
        item.addActionListener(cutAction.getActionListener());
        pop.add(item);
        pop.add(new JSeparator());
        item = new JMenuItem("Delete Nodes");
        RemoveNodesAction deleteAction = new RemoveNodesAction(this, filteredSelectedNodes);
        item.addActionListener(deleteAction.getActionListener());
        pop.add(item);
        pop.show(this, node.getX() + node.getWidth(), node.getY());
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void copyNodes() {
        if ((mSelectedNode != null) && (mSelectedNodes.size() == 0)) {
            CopyNodesAction copyAction = new CopyNodesAction(this, mSelectedNode);
            mSceneFlowEditor.setMessageLabelText("Node copied");
            copyAction.getActionListener();
            return;
        }

        if ((mSelectedNode == null) && (mSelectedNodes.size() > 0)) {
            CopyNodesAction copyAction = new CopyNodesAction(this, mSelectedNodes);
            String message = (mSelectedNodes.size() > 1) ? "Nodes copied" : "Node copied";
            mSceneFlowEditor.setMessageLabelText(mSelectedNodes.size() + message);
            copyAction.getActionListener();
            return;
        }
    }

    public void gobalContextMenu(MouseEvent evt) {
        JPopupMenu pop = new JPopupMenu();
        int nc = mClipboard.size();
        JMenuItem item = new JMenuItem((nc > 1) ? "Paste " + nc + " Nodes" : "Paste Node");
        PasteNodesAction pasteAction = new PasteNodesAction(this);
        item.addActionListener(pasteAction.getActionListener());
        pop.add(item);
        pop.show(this, evt.getX(), evt.getY());
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void increaseWorkSpaceLevel(Node node) {

        // Reset mouse interaction
        mIgnoreMouseInput = true;

        // synchronized (ActivityEventMulticaster.getInstance().mActivityEventListenerListLock) {
        removeEventListeners();
        mObservable.deleteObservers();
    //}
        //   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    // Clear the list of currently shown nodes and edges and
        // remove all components from the workspace. Additionally
        // clear the selected edges and nodes of the workspace.
        mNodeSet.clear();
        mEdgeSet.clear();
        mCmtSet.clear();
        mCmdBadgeMap.clear();

        removeAll();
        mSelectedEdge = null;
        mSelectedNode = null;
        mSelectedComment = null;
        mSelectedVariableBadge = null;
    // Push the current active supernode to the list
        // of active supernodes and add it's name to the path
        SuperNode superNode = (SuperNode) node.getDataNode();
        getSceneFlowManager().addActiveSuperNode(superNode);
        mSceneFlowEditor.addPathComponent(superNode.getName());
        // Create a new Gridmanager for the workspace
        mGridManager.update();
    // Show the nodes and supernodes on the workspace.
        // Show the edges on the workspace.
        // Show the variables on workspace.
        showNodesOnWorkSpace();
        showEdgesOnWorkSpace();
        showVariableBadge();
//        showVariablesOnWorkSpace(superNode);
        // Update the workspace
        revalidate();
        repaint();
    }
    
    public void selectNewWorkSpaceLevel(String name){ 
        if (getSceneFlowManager().getActiveSuperNodes().size() < 2) {
            return;
        }
        
        clearCurrentWorkspace(); 
 	 
        SuperNode parent = getSceneFlowManager().getCurrentActiveSuperNode(); 
        while (parent.getName().compareTo(name) != 0){ 
                decreaseWorkSpaceLevel(); 
                parent = getSceneFlowManager().getCurrentActiveSuperNode(); 
        } 
    } 

    public void decreaseWorkSpaceLevel() { 

        if (getSceneFlowManager().getActiveSuperNodes().size() < 2) { 
          return; 
        } 

        clearCurrentWorkspace(); 
                
        NodeSelectedEvent e = new NodeSelectedEvent(this, getSceneFlowManager().getCurrentActiveSuperNode()); 
        mEventCaster.convey(e); 
    }
    
    private void clearCurrentWorkspace() { 
        
        removeEventListeners(); 
        mObservable.deleteObservers(); 
        
        // Clear the list of currently shown nodes and edges and 
        // remove all components from the workspace. Additionally 
        // clear the selected edges and nodes of the workspace. 
        mNodeSet.clear(); 
        mEdgeSet.clear(); 
        mCmtSet.clear(); 
        mCmdBadgeMap.clear(); 
        removeAll(); 
        mSelectedEdge = null; 
        mSelectedNode = null; 
        mSelectedComment = null; 
        mSelectedVariableBadge = null; 
        // Pop the current active supernode from the list of 
        // active supernodes and remove it's name from the path 
        SuperNode s = getSceneFlowManager().removeActiveSuperNode(); 
        String str = mSceneFlowEditor.removePathComponent(); 
        // Create a new Gridmanager for the workspace 
        mGridManager.update(); 
        // Show the nodes and supernodes on the workspace. 
        // Show the edges on the workspace. 
        // Show the variables on workspace. 
        showNodesOnWorkSpace(); 
        showEdgesOnWorkSpace(); 
        showVariableBadge(); 
        // Update the workspace 
        revalidate(); 
        repaint(); 
    } 

 
    public void showNodesOnWorkSpace() {
        Vector<de.dfki.vsm.model.sceneflow.Node> nodeList
                = getSceneFlowManager().getCurrentActiveSuperNode().getNodeAndSuperNodeList();
        for (de.dfki.vsm.model.sceneflow.Node n : nodeList) {
            Point p = mGridManager.getNodeLocation(new Point(
                    n.getGraphics().getPosition().getXPos(),
                    n.getGraphics().getPosition().getYPos()));
            n.getGraphics().setPosition(p.x, p.y);
            Node guiNode = new Node(this, n);
            CmdBadge cmdBadge = new CmdBadge(guiNode);
            addNode(guiNode);
            addCmdBadge(guiNode, cmdBadge);
        }

        /**
         * ***
         *
         */
        Vector<de.dfki.vsm.model.sceneflow.Comment> commentList
                = getSceneFlowManager().getCurrentActiveSuperNode().getCommentList();
        for (de.dfki.vsm.model.sceneflow.Comment n : commentList) {
            add(new Comment(this, n));
        }

    }

    public void showEdgesOnWorkSpace() {
        for (Node sourceNode : mNodeSet) {
            Node targetNode = null;
            for (CEdge cedge : sourceNode.getDataNode().getCEdgeList()) {
                targetNode = getNode(cedge.getTarget());
                if (targetNode != null) {
          // Why should this be null????????
                    // Create a new GUI-Edge and add the new GUI-Edge to the workspace.
                    Edge edge = new Edge(this, cedge, Edge.TYPE.CEDGE, sourceNode, targetNode);
                    add(edge);
                }
            }
            for (PEdge pedge : sourceNode.getDataNode().getPEdgeList()) {
                targetNode = getNode(pedge.getTarget());
                if (targetNode != null) {
          // Why should this be null????????
                    // Create a new GUI-Edge and add the new GUI-Edge to the workspace.
                    Edge edge = new Edge(this, pedge, Edge.TYPE.PEDGE, sourceNode, targetNode);
                    add(edge);
                }
            }

            for (FEdge fedge : sourceNode.getDataNode().getFEdgeList()) {
                targetNode = getNode(fedge.getTarget());
                if (targetNode != null) {
          // Why should this be null????????
                    // Create a new GUI-Edge and add the new GUI-Edge to the workspace.
                    Edge edge = new Edge(this, fedge, Edge.TYPE.FEDGE, sourceNode, targetNode);
                    add(edge);
                }
            }

            for (IEdge iedge : sourceNode.getDataNode().getIEdgeList()) {
                targetNode = getNode(iedge.getTarget());
                if (targetNode != null) {
          // Why should this be null????????
                    // Create a new GUI-Edge and add the new GUI-Edge to the workspace.
                    Edge edge = new Edge(this, iedge, Edge.TYPE.IEDGE, sourceNode, targetNode);
                    add(edge);
                }
            }
            // Show the DEdge
            de.dfki.vsm.model.sceneflow.Edge dedge = sourceNode.getDataNode().getDedge();
            Edge.TYPE dEdgeType = null;
            if (dedge != null) {
                targetNode = getNode(dedge.getTarget());
                if (dedge instanceof EEdge) {
                    dEdgeType = Edge.TYPE.EEDGE;
                } else if (dedge instanceof TEdge) {
                    dEdgeType = Edge.TYPE.TEDGE;
                } else {
                    // Error
                }
                // Create a new GUI-Edge and add the new GUI-Edge to the workspace.
                Edge edge = new Edge(this, dedge, dEdgeType, sourceNode, targetNode);
                add(edge);
            }
      // Additionally update the appearance of the source c, which means
            // that we update the color and the end c markings of the c.
            //Editor.getInstance().update();
        }
    }

    private void deselectAllOtherComponents(JComponent comp) {
        if ((!comp.equals(mSelectedVariableBadge)) && (mSelectedVariableBadge != null)) {
            mSelectedVariableBadge.deSelect();
            mSelectedVariableBadge = null;
        }

        if ((!comp.equals(mSelectedComment)) && (mSelectedComment != null)) {
            mSelectedComment.setDeselected();
            mSelectedComment = null;
        }

        if ((!comp.equals(mSelectedNode)) && (mSelectedNode != null)) {
            mSelectedNode.setDeselected();
            mSelectedNode = null;
        }

        if ((!comp.equals(mSelectedEdge)) && (mSelectedEdge != null)) {
            mSelectedEdge.setDeselected();
            mSelectedEdge = null;
        }
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void mouseClicked(MouseEvent event) {
        mLastMousePosition = event.getPoint();
        //System.out.println("mouse clicked");
        if (mSelectTargetNodeMode) {
            try {
                createNewEdgeSelectTargetNode(event.getX(), event.getY());
            } catch (Exception e) {
                e.printStackTrace();
            }
            return;
        }
        
        // handle mouse click for area selections
        if (!mSelectedNodes.isEmpty()) {
            mDoAreaAction = false;
            Node clickedNode = null;
            for (Node node : mSelectedNodes) {
                if (node.containsPoint(event.getX(), event.getY())) {
                    clickedNode = node;
                    mDoAreaAction = true;
                }
            }

            if (!mDoAreaAction) {
                mDoAreaSelection = false;
                deselectAllNodes();
            } else {
                // show contect menu
                if (mSelectedNodes.size() > 1) {
                    if (event.getButton() == MouseEvent.BUTTON3 && event.getClickCount() == 1) {
                        multipleNodesContextMenu(event, clickedNode);
                    }
                    return;
                } else {
                    mSelectedNode = (Node) (mSelectedNodes.toArray())[0];
                    mDoAreaSelection = false;
                    deselectAllNodes();
                }
            }
        }

        // if there is a specific selected edge use it - much faster than checking all edges
        if (mSelectedEdge != null) {
            if (mSelectedEdge.mEg.curveContainsPoint(new Point(event.getX(), event.getY()))) {
                //System.out.println(mSelectedEdge.getType() + " clicked - (re) selected");
                mSelectedEdge.mouseClicked(event);
                return;
            } else {
                //System.out.println(mSelectedEdge.getType() + " not clicked - deselected");
                mSelectedEdge.setDeselected();
                mSelectedEdge = null;
            }
        }
        // if there is a specific selected c use it - much faster than checking all nodes
        if (mSelectedNode != null) {
            if (mSelectedNode.containsPoint(event.getX(), event.getY())) {
        // DEBUG System.out.println(mSelectedNode.getDataNode().getName() + " clicked - (re) selected");
                // tell c that it has been clicked
                mSelectedNode.mouseClicked(event);
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not clicked - deselected");
                mSelectedNode.setDeselected();
                mSelectedNode = null;
            }
        }
        // if there is a specific selected comment use it - much faster than checking all nodes
        if (mSelectedComment != null) {
            if (mSelectedComment.containsPoint(event.getPoint())) {
        // DEBUG System.out.println(mSelectedNode.getDataNode().getName() + " clicked - (re) selected");
                // tell c that it has been clicked
                mSelectedComment.mouseClicked(event);
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not clicked - deselected");
                mSelectedComment.setDeselected();
                mSelectedComment = null;
            }
        }
        // if there is a specific selected variable badge use it - much faster than checking all nodes
        if (mSelectedVariableBadge != null) {
            if (mSelectedVariableBadge.containsPoint(event.getPoint())) {
        // DEBUG System.out.println(mSelectedNode.getDataNode().getName() + " clicked - (re) selected");
                // tell c that it has been clicked
                mSelectedVariableBadge.mouseClicked(event);
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not clicked - deselected");
                mSelectedVariableBadge.deSelect();
                mSelectedVariableBadge = null;
            }
        }

        if (!mIgnoreMouseInput) {
            boolean entityClicked = false;
            // look if mouse click was on a c
            for (Node node : mNodeSet) {
                if (node.containsPoint(event.getX(), event.getY())) {
                    mSelectedNode = node;
                    // DEBUG System.out.println(mSelectedNode.getDataNode().getName() + " clicked - found and selected");
                    mSelectedNode.mouseClicked(event);
                    entityClicked = true;
                    return;
                }
            }
            // look if mouse click was on a edge
            for (Edge edge : mEdgeSet) {
                if (edge.mEg.curveContainsPoint(new Point(event.getX(), event.getY()))) {
                    mSelectedEdge = edge;
                    //System.out.println(mSelectedEdge.getType() + " clicked - found and selected");
                    mSelectedEdge.mouseClicked(event);
                    entityClicked = true;
                    return;
                }
            }
            // look if mouse click was on a comment
            for (Comment comment : mCmtSet) {
                if (comment.containsPoint(event.getPoint())) {
                    mSelectedComment = comment;
                    mSelectedComment.mouseClicked(event);
                    entityClicked = true;
                    return;
                }
            }
            // look of mouse click was on a variable badge
            if (mVarDisplay != null) {
                if (mVarDisplay.containsPoint(event.getPoint())) {
                    mSelectedVariableBadge = mVarDisplay;
                    mSelectedVariableBadge.mouseClicked(event);
                    entityClicked = true;
                    return;
                }
            }

            if (!entityClicked) {
                NodeSelectedEvent e = new NodeSelectedEvent(this, getSceneFlowManager().getCurrentActiveSuperNode());
                mEventCaster.convey(e);
            }
        } else {
            //System.out.println("mouse input ignored");
            mIgnoreMouseInput = false;
        }
        deselectAllNodes();

        // enable global context menu for clipbaord actions
        if (event.getButton() == MouseEvent.BUTTON3 && event.getClickCount() == 1) {
            if (mClipboard.size() > 0) {
                gobalContextMenu(event);
            }
        }

    }

    public void mousePressed(MouseEvent event) {
        mLastMousePosition = event.getPoint();
        //System.out.println("mouse pressed");

        if (mSelectTargetNodeMode) {
            try {
                createNewEdgeSelectTargetNode(event.getX(), event.getY());
            } catch (Exception e) {
                e.printStackTrace();
            }
            return;
        }
        // handle mouse pressed for area selections
        if (!mSelectedNodes.isEmpty()) {
            mDoAreaAction = false;
            Node clickedNode = null;
            for (Node node : mSelectedNodes) {
                if (node.containsPoint(event.getX(), event.getY())) {
                    clickedNode = node;
                    mDoAreaAction = true;
                }
            }

            if (!mDoAreaAction) {
                mDoAreaSelection = false;
                deselectAllNodes();
            } else {
                // show contect menu
                if (mSelectedNodes.size() > 1) {
                    if (event.getButton() == MouseEvent.BUTTON3 && event.getClickCount() == 1) {
                        multipleNodesContextMenu(event, clickedNode);
                    }
                    return;
                } else {
                    mSelectedNode = (Node) (mSelectedNodes.toArray())[0];
                    mDoAreaSelection = false;
                    deselectAllNodes();
                }
            }
        }
        // if there is a specific selected edge use it - much faster than checking all edges
        if (mSelectedEdge != null) {
            if (mSelectedEdge.mEg.curveContainsPoint(new Point(event.getX(), event.getY()))) {
                //System.out.println(mSelectedEdge.getType() + " pressed - (re) selected");
                mSelectedEdge.mousePressed(event);
                return;
            } else {
                //System.out.println(mSelectedEdge.getType() + " not pressed - deselected");
                mSelectedEdge.setDeselected();
                mSelectedEdge = null;
            }
        }
        // if there is a specific selected c use it - much faster than checking all nodes
        if (mSelectedNode != null) {
            if (mSelectedNode.containsPoint(event.getX(), event.getY())) {
        //System.out.println(mSelectedNode.getDataNode().getName() + " pressed");
                // tell c that it has been clicked
                mSelectedNode.mousePressed(event);
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not pressed - deselected");
                mSelectedNode.setDeselected();
                mSelectedNode = null;
            }
        }
        // if there is a specific selected comment use it - much faster than checking all nodes
        if (mSelectedComment != null) {
            if (mSelectedComment.containsPoint(event.getPoint())) {
        //System.out.println(mSelectedNode.getDataNode().getName() + " pressed");
                // tell c that it has been clicked
                mSelectedComment.mousePressed(event);
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not pressed - deselected");
                mSelectedComment.setDeselected();
                mSelectedComment = null;
            }
        }
        // if there is a specific selected variable badge use it - much faster than checking all nodes
        if (mSelectedVariableBadge != null) {
            if (mSelectedVariableBadge.containsPoint(event.getPoint())) {
        // DEBUG System.out.println(mSelectedNode.getDataNode().getName() + " clicked - (re) selected");
                // tell c that it has been clicked
                mSelectedVariableBadge.mouseClicked(event);
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not clicked - deselected");
                mSelectedVariableBadge.deSelect();
                mSelectedVariableBadge = null;
            }
        }
    //
        // Fall back cases - lookup
        //
        // look if mouse pressed (without a click) was on a c
        for (Node node : mNodeSet) {
            if (node.containsPoint(event.getX(), event.getY())) {
                mSelectedNode = node;
                deselectAllOtherComponents(mSelectedNode);

                //System.out.println(mSelectedNode.getDataNode().getName() + " pressed - found and pressed");
                mSelectedNode.mousePressed(event);
                return;
            }
        }

        // look if mouse click was on a edge
        for (Edge edge : mEdgeSet) {
            if (edge.mEg.curveContainsPoint(new Point(event.getX(), event.getY()))) {
                mSelectedEdge = edge;
                deselectAllOtherComponents(mSelectedEdge);
                //System.out.println(mSelectedEdge.getType() + " pressed - found and selected");
                mSelectedEdge.mousePressed(event);
                return;
            }
        }
        // look if mouse click was on a comment
        for (Comment comment : mCmtSet) {
            if (comment.containsPoint(event.getPoint())) {
                mSelectedComment = comment;
                deselectAllOtherComponents(mSelectedComment);
                mSelectedComment.mousePressed(event);
                return;
            }
        }
        // look of mouse click was on a variable badge
        if (mVarDisplay != null) {
            if (mVarDisplay.containsPoint(event.getPoint())) {
                mSelectedVariableBadge = mVarDisplay;
                mSelectedVariableBadge.mousePressed(event);
                return;
            }
        }

        deselectAllNodes();

        // enable global context menu for clipbaord actions
        if (event.getButton() == MouseEvent.BUTTON3 && event.getClickCount() == 1) {
            if (mClipboard.size() > 0) {
                gobalContextMenu(event);
            }
            return;
        }

        // get point as possible point for area selection!
        mAreaSelection.x = event.getX();
        mAreaSelection.width = event.getX();
        mAreaSelection.y = event.getY();
        mAreaSelection.height = event.getY();
    }

    public void mouseReleased(MouseEvent event) {
        if (mDoAreaSelection) {
            mDoAreaSelection = false;
            repaint();
            return;
        }

        if (mSelectTargetNodeMode) {
            try {
                createNewEdgeSelectTargetNode(event.getX(), event.getY());
            } catch (Exception e) {
                e.printStackTrace();
            }
            return;
        }

        if (mDoAreaAction) {
            for (Node node : mSelectedNodes) {
                Point p = node.getLocation();
                // check location of each c
                if (node.mDragged) {
                    node.resetLocation(mGridManager.getNodeLocation(p));
                     
                }
        // update workspace area - if dragged beyond current borders
                // sWorkSpaceDrawArea = getSize();
                node.mouseReleased(event);
                repaint();
            }
            return;
        }

        // if there is a specific selected edge use it - much faster than checking all edges
        if (mSelectedEdge != null) {
            if (mSelectedEdge.mEg.curveContainsPoint(event.getPoint())) {

                // if the edge can be connected to an other node, do so!
                if (mEdgeTargetNodeReassign) {
                    new DeflectEdgeAction(this, mSelectedEdge, mReassignNode, event.getPoint()).run();
                    mEdgeTargetNodeReassign = false;
                    mReassignNode = null;
                    return;
                }

                mSelectedEdge.mouseReleased(event);
                mGridManager.normalizeGridWeight();
                return;
            } else {
                //System.out.println(mSelectedEdge.getType() + " not released - deselected");
                mSelectedEdge.setDeselected();
                mSelectedEdge = null;
            }
        }

        // if there is a specific selected c use it - much faster than checking all nodes
        if (mSelectedNode != null) {
            if (mSelectedNode.containsPoint(event.getX(), event.getY())) {
        //System.out.println(mSelectedNode.getDataNode().getName() + " released");
                // tell c that it has been clicked
                // let the gridmanager do a repositioning if c has been dragged
                if (mSelectedNode.mDragged) {
                    Point p = mSelectedNode.getLocation();
                    mSelectedNode.resetLocation(mGridManager.getNodeLocation(p));
                    
                    // DEBUG
                    System.out.println("node " + mSelectedNode.getDataNode().getName() +  " dragged to: " +  mSelectedNode.getX()+ ", " +  mSelectedNode.getY());
                    
                    // Update sceneflow with new node position
                    mSelectedNode.getDataNode().getGraphics().setPosition(mSelectedNode.getX(),mSelectedNode.getY());
                   
          // update workspace area - if dragged beyond current borders
                    //sWorkSpaceDrawArea = getSize();
                }
                mSelectedNode.mouseReleased(event);
                revalidate();
                repaint();
                mGridManager.normalizeGridWeight();
                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not released - deselected");
                mSelectedNode.setDeselected();
                mSelectedNode = null;
            }
        }

        // if there is a specific selected comment use it - much faster than checking all nodes
        if (mSelectedComment != null) {
            if (mSelectedComment.containsPoint(event.getPoint())) {
                mSelectedComment.mouseReleased(event);
                revalidate();
                repaint();
                return;
            } else {
        //System.out.println(mSelectedNode.getDataNode().getName() + " not released - deselected");
                //mSelectedComment.setDeselected();
                mSelectedComment = null;
            }

            // finally do a repaint
            repaint();
        }
    }

    public void mouseDragged(MouseEvent event) {
        if (mSelectTargetNodeMode) {
            try {
                createNewEdgeSelectTargetNode(event.getX(), event.getY());
                mSelectedEdge = null;
                mSelectedNode = null;
            } catch (Exception e) {
                e.printStackTrace();
            }
            return;
        }

        // if there is a specific selected edge use it - much faster than checking all edges
        if (mSelectedEdge != null) {
            //DEBUG //System.out.println("EDGE SELECTED!");
            if (mSelectedEdge.mCP1Selected || mSelectedEdge.mCP2Selected || mSelectedEdge.mCSPSelected || mSelectedEdge.mCEPSelected) {

                if (mSelectedEdge.mCSPSelected) {
                    // look if mouse pressed (without a click) was on a c
                    mEdgeSourceNodeReassign = false;
                    for (Node node : mNodeSet) {
                        if (node != mSelectedEdge.getSourceNode()) {
                            if (node.containsPoint(event.getX(), event.getY())) {
                                mReassignNode = node;
                                mEdgeSourceNodeReassign = true;
                                break;
                            }
                        }
                    }
                }

                if (mSelectedEdge.mCEPSelected) {
                    // look if mouse pressed (without a click) was on a c
                    mEdgeTargetNodeReassign = false;
                    for (Node node : mNodeSet) {
                        if (node != mSelectedEdge.getTargetNode()) {
                            if (node.containsPoint(event.getX(), event.getY())) {
                                mReassignNode = node;
                                mEdgeTargetNodeReassign = true;
                                break;
                            }
                        }
                    }
                }

                mSelectedEdge.mouseDragged(event);
                revalidate();
                repaint();
                return;
            }
        }

        // moving multiple nodes those which are selected before
        if (mDoAreaAction) {
            // compute movement trajectory vectors
            Point currentMousePosition = event.getPoint();
            Point mouseMoveVector = new Point(currentMousePosition.x - mLastMousePosition.x, currentMousePosition.y - mLastMousePosition.y);
            mLastMousePosition = new Point(currentMousePosition.x, currentMousePosition.y);

            dragNodes(mSelectedNodes, event, mouseMoveVector);
            return;
        }

        // if there is a specific selected c use it - much faster than checking all nodes
        if (mSelectedNode != null) {
            if (mSelectedNode.mPressed) {
                // compute movement trajectory vectors
                Point currentMousePosition = event.getPoint();
                Point mouseMoveVector = new Point(currentMousePosition.x - mLastMousePosition.x, currentMousePosition.y - mLastMousePosition.y);
                mLastMousePosition = new Point(currentMousePosition.x, currentMousePosition.y);

                dragNode(mSelectedNode, event, mouseMoveVector);

                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not dragged - deselected");
                mSelectedNode.setDeselected();
                mSelectedNode = null;
            }
        }

        // if there is a specific selected comment use it
        if (mSelectedComment != null) {
            Point currentMousePosition = event.getPoint();
            // if not dragged, but once resized, leave it by resized and vice versa, leave it by dragged
            if (!mSelectedComment.mDragged) {
                mSelectedComment.mResizing = mSelectedComment.isResizingAreaSelected(currentMousePosition);
            }
            if (mSelectedComment.mPressed) {
                // compute movement trajectory vectors

                Point mouseMoveVector = new Point(currentMousePosition.x - mLastMousePosition.x, currentMousePosition.y - mLastMousePosition.y);
                mLastMousePosition = new Point(currentMousePosition.x, currentMousePosition.y);

                if (mSelectedComment.mResizing) {
                    resizeComment(mSelectedComment, event, mouseMoveVector);
                } else {
                    dragComment(mSelectedComment, event, mouseMoveVector);
                }

                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not dragged - deselected");
                mSelectedComment = null;
            }
        }

        // if there is a specific selected comment use it
        if (mSelectedVariableBadge != null) {
            Point currentMousePosition = event.getPoint();

            if (mSelectedVariableBadge.mSelected) {
                // compute movement trajectory vectors

                Point mouseMoveVector = new Point(currentMousePosition.x - mLastMousePosition.x, currentMousePosition.y - mLastMousePosition.y);
                mLastMousePosition = new Point(currentMousePosition.x, currentMousePosition.y);

                dragVariableBadge(mSelectedVariableBadge, event, mouseMoveVector);

                return;
            } else {
                //System.out.println(mSelectedNode.getDataNode().getName() + " not dragged - deselected");
                mSelectedVariableBadge = null;
            }
        }

        // mouse interaction has to be the selection of an area ...
        mDoAreaSelection = true;
        mAreaSelection.width = event.getX() - mAreaSelection.x;
        mAreaSelection.height = event.getY() - mAreaSelection.y;
        selectNodesInArea();
        repaint();
    }

    public void mouseEntered(MouseEvent event) {
    }

    public void mouseExited(MouseEvent event) {
    }

    public void mouseMoved(MouseEvent event) {
        if (mSelectTargetNodeMode) {
            mSelectNodePoint = event.getPoint();

            for (Node node : mNodeSet) {
                if (node.containsPoint(mSelectNodePoint.x, mSelectNodePoint.y)) {
                    node.hightlightNode();
                    break;
                }
            }

            repaint();
            return;
        }

        if ((event.getModifiersEx() == 128)) {
            for (Node node : mNodeSet) {
                if (node.containsPoint(event.getX(), event.getY())) {
                    if (mNodeVariableDisplay != null) {
                        remove(mNodeVariableDisplay);
                    }
                    showNodeVariables(node);
                    repaint();
                    return;
                }
            }
        } else if (mNodeVariableDisplay != null) {
            remove(mNodeVariableDisplay);
            mNodeVariableDisplay = null;
            repaint();
        }
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    private void dragNode(Node node, MouseEvent event, Point moveVec) {
        boolean validDragging = true;
        Point nodePos = node.getLocation();
        if (((nodePos.x + moveVec.x) <= 0) || ((nodePos.y + moveVec.y) <= 0)) {
            validDragging = false;
        }

        if (validDragging) {
            Point nodeLoc = node.getLocation();
            mGridManager.freeGridPosition(nodeLoc);
            node.updateLocation(moveVec);
            if ((event.getModifiersEx() == 1024)) {
                node.mDragged = true;
            }
            //sWorkSpaceDrawArea = getSize();
            revalidate();
            repaint();
        }
    }

    private void dragComment(Comment comment, MouseEvent event, Point moveVec) {
        boolean validDragging = true;

        Point commentPos = comment.getLocation();
        if (((commentPos.x + moveVec.x) <= 0) || ((commentPos.y + moveVec.y) <= 0)) {
            // stop dragging, if upper and left border would be passed!
            validDragging = false;
        }

        if (validDragging) {
            comment.updateLocation(moveVec);
            if ((event.getModifiersEx() == 1024)) {
                comment.mDragged = true;
            }
            revalidate();
            repaint();
        }
    }

    private void dragVariableBadge(VarBadge vb, MouseEvent event, Point moveVec) {
        boolean validDragging = true;

        Point vbPos = vb.getLocation();
        if (((vbPos.x + moveVec.x) <= 0) || ((vbPos.y + moveVec.y) <= 0)) {
            // stop dragging, if upper and left border would be passed!
            validDragging = false;
        }

        if (validDragging) {
            vb.updateLocation(moveVec);
            if ((event.getModifiersEx() == 1024)) {
                vb.mDragged = true;
            }
            revalidate();
            repaint();
        }
    }

    private void resizeComment(Comment comment, MouseEvent event, Point moveVec) {
        Point nodePos = comment.getLocation();

        comment.resize(moveVec);

        if ((event.getModifiersEx() == 1024)) {
            comment.mDragged = true;
        }
        revalidate();
        repaint();
    }

    private void dragNodes(Set<Node> nodes, MouseEvent event, Point moveVec) {
        boolean validDragging = true;

        for (Node node : nodes) {
            Point nodePos = node.getLocation();
            if (((nodePos.x + moveVec.x) <= 0) || ((nodePos.y + moveVec.y) <= 0)) {
                // stop dragging, if upper and left border would be passed!
                validDragging = false;
            }
        }

        if (validDragging) {
            for (Node node : nodes) {
                Point nodeLoc = node.getLocation();
                mGridManager.freeGridPosition(nodeLoc);
                node.updateLocation(moveVec);
                if ((event.getModifiersEx() == 1024)) {
                    node.mDragged = true;
                }
                //sWorkSpaceDrawArea = getSize();
                revalidate();
                repaint();
            }
        }
    }
    
    /*************************************************************************** 
    * Implementation of the delete button. 
    * the del-key is bound to the function mWorkspace.deleteSelectedItem 
    * this detects which items are selected and will throw them away 
    * selection will be canceled. 
    * 1-2-2014 Bert Bierman TNO 
    * 
    **************************************************************************/ 
    private void setKeyBindings() { 
        ActionMap actionMap = getActionMap(); 
        int condition = JComponent.WHEN_IN_FOCUSED_WINDOW; 
        InputMap inputMap = getInputMap(condition ); 

        String vkDel = "VK_DEL"; 
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), vkDel); 

        actionMap.put(vkDel, new KeyAction(this)); 

     } 

    private class KeyAction extends AbstractAction { 
           WorkSpace mWorkspace = null; 

       public KeyAction(WorkSpace workspace) { 
           mWorkspace = workspace; 
       } 

    @Override 
    public void actionPerformed(ActionEvent actionEvt) { 
          //System.out.println(actionEvt.getActionCommand() + " pressed"); 
           mWorkspace.deleteSelectedItem(); 
       } 
    } 

    public void deleteSelectedItem(){       
        if (mSelectedEdge != null) { 
                 removeEdge(); 
        } 
        if (!mSelectedNodes.isEmpty()) { 
                removeNodes(); 
        } 
        if (mSelectedNode != null) { 
                 removeNode(); 
        } 
    } 

    private void removeEdge(){ 
        mSelectedEdge.mIsSelected = false; 
        RemoveEdgeAction deleteAction = new RemoveEdgeAction(this, mSelectedEdge); 
        deleteAction.run(); 
    } 

    private void removeNodes() { 
      for (Node node : mNodeSet) { 
          node.mSelected = false; 
      } 
          RemoveNodesAction deleteAction = new RemoveNodesAction(this, mSelectedNodes); 
          deleteAction.run(); 
    } 

    private void removeNode() { 
      mSelectedNode.mSelected = false; 
          RemoveNodeAction deleteAction = new RemoveNodeAction(this, mSelectedNode); 
          deleteAction.run(); 
    } 


    /*************************************************************************** 
    * 
    * 
    * 
    **************************************************************************/ 

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    @Override
    public void paintComponent(Graphics g) {
        //mLogger.message("Drawing Workspace");
        Graphics2D g2d = (Graphics2D) g;

        if (mSelectTargetNodeMode) {
            setBackground(Color.LIGHT_GRAY);
        } else {
            setBackground(Color.WHITE);
        }

        super.paintComponent(g);

        mGridManager.drawGrid(g2d);

        if (mDoAreaSelection) {
            mDrawArea.x = (mAreaSelection.width > 0) ? mAreaSelection.x : mAreaSelection.x + mAreaSelection.width;
            mDrawArea.y = (mAreaSelection.height > 0) ? mAreaSelection.y : mAreaSelection.y + mAreaSelection.height;
            mDrawArea.width = Math.abs(mAreaSelection.width);
            mDrawArea.height = Math.abs(mAreaSelection.height);
            g2d.draw(mDrawArea);
        }

        Color indicator = Color.WHITE;

        switch (getSceneFlowManager().getCurrentActiveSuperNode().getFlavour()) {
            case CNODE:
                indicator = sCEDGE_COLOR;
                break;
            case PNODE:
                indicator = sPEDGE_COLOR;
                break;
            case FNODE:
                indicator = sFEDGE_COLOR;
                break;
            case INODE:
                indicator = sIEDGE_COLOR;
                break;
            case TNODE:
                indicator = sTEDGE_COLOR;
                break;
        }

        g2d.setColor(indicator);
        g2d.setStroke(new BasicStroke(3.0f));
        g2d.drawRect(1, 1, getSize().width - 3, getSize().height - 4);

        // draw line between source c and current mouse position
        if (mSelectTargetNodeMode) {
            if (mSelectNodePoint != null) {
                Point sourceNodeCenter = mEdgeSourceNode.getCenterPoint();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2d.setColor(Color.WHITE);
                g2d.setStroke(new BasicStroke(3.0f));
                g2d.drawLine(sourceNodeCenter.x, sourceNodeCenter.y, mSelectNodePoint.x, mSelectNodePoint.y);

                TextLayout textLayout = new TextLayout(sEdgeCreationHint.getIterator(), g2d.getFontRenderContext());
                int height = (int) (textLayout.getAscent() + textLayout.getDescent() + textLayout.getLeading());
                int width = (int) textLayout.getVisibleAdvance();

                g2d.setStroke(new BasicStroke(0.5f));
                g2d.drawLine(mSelectNodePoint.x, mSelectNodePoint.y, mSelectNodePoint.x, mSelectNodePoint.y - (sNODEHEIGHT / 2) + (height / 2));

                g2d.setColor(new Color(100, 100, 100, 100));
                g2d.fillRoundRect(mSelectNodePoint.x - (width / 2) - 5, mSelectNodePoint.y - (sNODEHEIGHT / 2) - (height / 2) - 6, width + 10, height + 5, 5, 5);
                g2d.setColor(Color.WHITE);
                g2d.setStroke(new BasicStroke(2.0f));
                g2d.drawRoundRect(mSelectNodePoint.x - (width / 2) - 5, mSelectNodePoint.y - (sNODEHEIGHT / 2) - (height / 2) - 6, width + 10, height + 5, 5, 5);
                g2d.drawString(sEdgeCreationHint.getIterator(), mSelectNodePoint.x - (width / 2), mSelectNodePoint.y - (sNODEHEIGHT / 2) + 1);
            }
        }

    }
}
