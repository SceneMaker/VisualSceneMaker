package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceAdapter;
import java.awt.dnd.DragSourceListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.ToolTipManager;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import static de.dfki.vsm.editor.util.Preferences.sBASICNODE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sCEDGE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sCOMMENT_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sEEDGE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sFEDGE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sIEDGE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sPEDGE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sROOT_FOLDER;
import static de.dfki.vsm.editor.util.Preferences.sSUPERNODE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sTEDGE_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sFUNCTION_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sFUNCTION_ERROR_ENTRY;


///***************************************************************************
// *
// *
// *
// **************************************************************************/
//class Entry extends DefaultMutableTreeNode implements Transferable {
//
//    private final String mText;
//    private final Icon mIcon;
//    private final Object mData;
//
//    public Entry(String text, Icon icon, Object data) {
//        mText = text;
//        mIcon = icon;
//        mData = data;
//    }
//
//    public String getText() {
//        return mText;
//    }
//
//    public Icon getIcon() {
//        return mIcon;
//    }
//
//    public Object getData() {
//        return mData;
//    }
//
//    public Object getTransferData(DataFlavor flavor) throws
//            UnsupportedFlavorException, IOException {
//        return mData;
//    }
//
//    public boolean isDataFlavorSupported(DataFlavor flavor) {
//        return true;
//    }
//
//    public DataFlavor[] getTransferDataFlavors() {
//        return null;
//    }
//}
/**
 * *************************************************************************
 *
 *
 *
 ***************************************************************************
 */
class ElementTree extends JTree implements Observer, EventListener, ActionListener { 

    private final TreeEntry mRootEntry = new TreeEntry("SceneFlow", sROOT_FOLDER, null);
    private final TreeEntry mBasicEntry = new TreeEntry("Elements", null, null);
    private final TreeEntry mSceneEntry = new TreeEntry("Scenes", null, null);
    private final TreeEntry mFunDefEntry = new TreeEntry("Functions", null, null);
    private final TreeEntry mSuperNodeEntry = new TreeEntry("Super Node", sSUPERNODE_ENTRY, Node.Type.SuperNode);
    private final TreeEntry mBasicNodeEntry = new TreeEntry("Basic Node", sBASICNODE_ENTRY, Node.Type.BasicNode);
    private final TreeEntry mEEdgeEntry = new TreeEntry("Epsilon Edge", sEEDGE_ENTRY, new Edge(Edge.TYPE.EEDGE));
    private final TreeEntry mTEdgeEntry = new TreeEntry("Timeout Edge", sTEDGE_ENTRY, new Edge(Edge.TYPE.TEDGE));
    private final TreeEntry mPEdgeEntry = new TreeEntry("Probability Edge", sPEDGE_ENTRY, new Edge(Edge.TYPE.PEDGE));
    private final TreeEntry mCEdgeEntry = new TreeEntry("Conditional Edge", sCEDGE_ENTRY, new Edge(Edge.TYPE.CEDGE));
    private final TreeEntry mIEdgeEntry = new TreeEntry("Interruptive Edge", sIEDGE_ENTRY, new Edge(Edge.TYPE.IEDGE));
    private final TreeEntry mFEdgeEntry = new TreeEntry("Fork Edge", sFEDGE_ENTRY, new Edge(Edge.TYPE.FEDGE));
    private final TreeEntry mCommentEntry = new TreeEntry("Comment", sCOMMENT_ENTRY, new Comment());
    
    private final JMenuItem functionsAdd = new JMenuItem("Add...");
    private final JMenuItem functionModify = new JMenuItem("Modify...");
    private final JMenuItem functionRemove = new JMenuItem("Remove");
    
    private final SceneFlow mSceneFlow;
  
    // Drag & Drop support
    private DragSource mDragSource;
    private DragGestureListener mDragGestureListener;
    private DragSourceListener mDragSourceListener;
    private int mAcceptableDnDActions;
    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventCaster = EventCaster.getInstance();
    //private final EventCaster mEventMulticaster = EventCaster.getInstance();
    // private final Observable mObservable = new Observable();

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
//    public class Observable extends java.util.Observable {
//
//        public void update(Object obj) {
//            setChanged();
//            notifyObservers(obj);
//        }
//    }
    @Override
    public void update(java.util.Observable obs, Object obj) {
        //mLogger.message("ElementTree.update(" + obj + ")");
        if (obj instanceof ProjectData) {
            updateScenes((ProjectData) obj);
            updateFunDefs((ProjectData) obj);
        }
        // Update the visual appearance of the ElementTree
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
    private class CellRenderer extends DefaultTreeCellRenderer {

        public CellRenderer() {
            super();
            //setBackgroundNonSelectionColor(UIManager.getColor("Panel.background"));
        }

        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                boolean selection, boolean expanded,
                boolean leaf, int row, boolean hasFocus) {
            super.getTreeCellRendererComponent(tree, value, selection, expanded, leaf, row, hasFocus);
            // Get the entry information
            Object data = ((TreeEntry) value).getData();
            String text = ((TreeEntry) value).getText();
            Icon icon = ((TreeEntry) value).getIcon();
            // Render the cell
            setText(text);
            if (icon != null) {
                setIcon(icon);
            }
            setBackgroundNonSelectionColor(Color.WHITE);
            setBackgroundSelectionColor(Color.LIGHT_GRAY);
            setBorderSelectionColor(Color.LIGHT_GRAY);
            return this;
        }
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    public ElementTree(SceneFlow sceneFlow) {
        super(new DefaultTreeModel(null));
        //
        mSceneFlow = sceneFlow;
        setBorder(BorderFactory.createEmptyBorder());
        setCellRenderer(new CellRenderer());
        setBackground(Color.WHITE);
        setRootVisible(false);
        // setRowHeight(0);
        //
        initDnDSupport();
        initComponents();
        addMouseListener(getMouseAdapter(this));
        ToolTipManager.sharedInstance().registerComponent(this);
        //
        expandAll();
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void expandAll() {
        for (int i = 0; i < getRowCount(); i++) {
            expandRow(i);
        }
        updateUI();
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
        mBasicEntry.add(mSuperNodeEntry);
        mBasicEntry.add(mBasicNodeEntry);
        mBasicEntry.add(mEEdgeEntry);
        mBasicEntry.add(mTEdgeEntry);
        mBasicEntry.add(mPEdgeEntry);
        mBasicEntry.add(mCEdgeEntry);
        mBasicEntry.add(mIEdgeEntry);
        mBasicEntry.add(mFEdgeEntry);
        mBasicEntry.add(mCommentEntry);
        //
        mRootEntry.add(mBasicEntry);
        mRootEntry.add(mSceneEntry);
        mRootEntry.add(mFunDefEntry);
        //
        ((DefaultTreeModel) getModel()).setRoot(mRootEntry);
        
        functionsAdd.addActionListener(this);
        functionModify.addActionListener(this);
        functionRemove.addActionListener(this);
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    private void updateScenes(ProjectData project) {
        //
        mSceneEntry.removeAllChildren();
        //
        SceneScript sceneScript = project.getSceneScript();
        if (sceneScript != null) {
            for (SceneGroup group : sceneScript.getOrderedGroupSet().descendingSet()) {
                mSceneEntry.add(new TreeEntry(group.getName() + " (" + group.getSize() + ")", null, group)); //PG: added a space before the number cnt of scenes in scenegroup for better readability
            }
        }
        //
        expandAll();
    }
    
    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */
    public MouseAdapter getMouseAdapter (final JTree tree) {
        return new MouseAdapter ()
        {
                       
            public void mousePressed (MouseEvent e) {
                
                TreePath path = tree.getPathForLocation(e.getX(), e.getY());
                if (path == null) {
                    return;
                }

                tree.setSelectionPath(path);
                boolean showPopup = false;
                JPopupMenu menu = new JPopupMenu();
                int pathCount = path.getPathCount();
                
                // TODO: why do we check the pathCount?
                // test if the user clicked on the Functions entry
                if (pathCount == 2) {                    
                    if (path.getLastPathComponent().equals(mFunDefEntry)) {
                        if (e.isPopupTrigger()) {
                            menu.add(functionsAdd);
                            showPopup = true;
                        }
                    }
                } 

                // test if the user clicked on exact function
                else if (pathCount == 3) {
                    
                    TreePath parentPath = path.getParentPath();
                    if (parentPath.getLastPathComponent().equals(mFunDefEntry)) {
                        
                        FunDef selectedDef =  (FunDef)((TreeEntry) path.getLastPathComponent()).getData();
                        
                        launchFunctionEvent(selectedDef);
                       
                      
                        if (e.isPopupTrigger()) {
                            menu.add(functionModify);
                            menu.add(functionRemove);
                            showPopup = true;
                        }
                        else if (e.getClickCount() == 2 && !e.isConsumed()) {
                            TreeEntry entry = (TreeEntry) path.getLastPathComponent();
                            modifyFunctionAction(entry);
                        }
                    }
                }

                if (showPopup) {
                    menu.show (tree, e.getX(), e.getY());
                }
            }

            private void launchFunctionEvent(FunDef funDef) {                
                try{ 
                    FunctionSelectedEvent ev = new FunctionSelectedEvent(this, funDef);
                    mEventCaster.convey(ev);
                }
                catch(Exception e){
                    
                }                
            }
        };
    }
    
    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */  
    private final void modifyFunctionAction (final TreeEntry entry) {
        if (entry != null) {
            FunDef usrCmdDef = new FunDefDialog((FunDef) entry.getData()).run();
            if (usrCmdDef != null) {
                FunDef oldFunDef = (FunDef) entry.getData();
                mSceneFlow.removeUsrCmdDef(oldFunDef.getName());
                mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
                updateFunDefs();
          }
        }
    }  

    

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */ 
    private void updateFunDefs(ProjectData project) {
        //
        mFunDefEntry.removeAllChildren();
        //
        SceneFlow sceneFlow = project.getSceneFlow();
        for (FunDef def : sceneFlow.getUsrCmdDefMap().values()) {
            mFunDefEntry.add(new TreeEntry(def.getName(), null, def));
        }
        //
        expandAll();
    }
    
    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */ 
    private String getEntryName (final TreeEntry entry) {
        if (entry != null) {       
            FunDef oldFunDef = (FunDef) entry.getData();
            return oldFunDef.getName();                 
        }
        return null;
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */ 
    public void updateFunDefs() {
        mFunDefEntry.removeAllChildren();

        List<FunDef> functionDefinitions = new ArrayList<FunDef>(
                mSceneFlow.getUsrCmdDefMap().values());
        Collections.sort(functionDefinitions);

        for (final FunDef def : functionDefinitions) {
          mFunDefEntry.add(new TreeEntry(def.getName(), def.isValidClass() ? sFUNCTION_ENTRY : sFUNCTION_ERROR_ENTRY, def));
        } 
    }

    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */ 
    @Override
    public void actionPerformed (ActionEvent e) {
        Object source = e.getSource();

        if (source == functionsAdd) {
            FunDef usrCmdDef = new FunDefDialog(null).run();
            if (usrCmdDef != null) {
                mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
                updateFunDefs();
                Editor.getInstance().update();
                launchFunctionEvent(usrCmdDef);
                      
            }
        } else if (source == functionModify) {
            TreeEntry entry = (TreeEntry) getLastSelectedPathComponent(); 
            modifyFunctionAction(entry);
            Editor.getInstance().update();
            launchFunctionEvent((FunDef) entry.getData());
              
        } else if (source == functionRemove) {
            TreeEntry entry = (TreeEntry) getLastSelectedPathComponent(); 
            if (entry != null) {
                FunDef oldFunDef = (FunDef) entry.getData();
                mSceneFlow.removeUsrCmdDef(oldFunDef.getName());
                updateFunDefs();
                Editor.getInstance().update();
                launchFunctionEvent((FunDef) entry.getData());              
            }
        }
    }
    
    private void launchFunctionEvent(FunDef funDef) {                
        try{ 
            FunctionSelectedEvent ev = new FunctionSelectedEvent(this, funDef);
            mEventCaster.convey(ev);
        }
        catch(Exception e){
        }                
    }    
            
    /**
     * ***********************************************************************
     *
     *
     *
     *************************************************************************
     */ 
    private void initDnDSupport() {
        // Create the default drag source
        mDragSource = DragSource.getDefaultDragSource();
        // Install the drag source listener
        mDragSourceListener = new DragSourceAdapter() {};
        // Install the drag gesture listener
        mDragGestureListener = new DragGestureListener() {

            @Override
            public void dragGestureRecognized(DragGestureEvent event) {
                // TODO: NULLPOINTEREXCEPTION abfangen
                TreeEntry selectedEntry = (TreeEntry) getSelectionPath().getLastPathComponent();
                mDragSource.startDrag(event, DragSource.DefaultCopyDrop, selectedEntry, mDragSourceListener);
            }
        };
        // Set the acceptable actions
        mAcceptableDnDActions = DnDConstants.ACTION_COPY;
        // Set the default drag gesture recognizer
        mDragSource.createDefaultDragGestureRecognizer(this, mAcceptableDnDActions, mDragGestureListener);
    }
}

/**
 * ***********************************************************************
 *
 *
 *
 *************************************************************************
 */
public class ElementDisplay extends JScrollPane implements Observer, EventListener {

    private final Observable mObservable = new Observable();
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventMulticaster = EventCaster.getInstance();
    private final ElementTree mElementTree;

    private class Observable extends java.util.Observable {

        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    @Override
    public void update(java.util.Observable obs, Object obj) {
        //mLogger.message("ElementDisplay.update(" + obj + ")");
        mObservable.update(obj);
        updateFunctionList();
    }

    private void updateFunctionList(){
        mElementTree.updateFunDefs();
    }
   
    @Override
    public void update(EventObject event) {       
    }
    
    public ElementTree getElementTree() {                
        return mElementTree;
    }

    public ElementDisplay(SceneFlow sceneFlow) {
        
        mElementTree = new ElementTree(sceneFlow);
        mObservable.addObserver(mElementTree);
        //
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEtchedBorder());
        setPreferredSize(new Dimension(250, 200));

        setViewportView(mElementTree);
    }
}
