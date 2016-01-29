package de.dfki.vsm.editor.project.sceneflow.elements;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.TreeEntry;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.dialog.DialogActAttributes;
import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.event.DialogActSelectedEvent;
import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.FunctionModifiedEvent;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.editor.event.SceneSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.dialogact.DialogAct;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.runtime.dialogacts.DialogActInterface;
import de.dfki.vsm.util.evt.EventDispatcher;
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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.ToolTipManager;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 * @author Gregor Mehlmann
 */
public final class SceneFlowElementPanel extends JScrollPane {

    // The singelton logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The element tree of this panel
    private final ElementTree mElementTree;

    // Construct the element display
    public SceneFlowElementPanel(final EditorProject project) {
        // Initialize the element tree
        mElementTree = new ElementTree(project);
        // Initialize the GUI components
        setBackground(Color.WHITE);
        setViewportView(mElementTree);
        setBorder(BorderFactory.createEtchedBorder());
        setPreferredSize(new Dimension(230, 200));
    }
    
    public void expandTree(){
        mElementTree.expandAllRecursive();
    }

    // Refresh the element display
    public final void refresh() {
        // Print some information
        //mLogger.message("Refreshing '" + this + "'");
        // Refresh the element tree
        mElementTree.refresh();
    }
}

/**
 * @author Gregor Mehlmann
 */
class ElementTree extends JTree implements ActionListener, TreeSelectionListener {
    //List of scene elements
    private final ArrayList<TreeEntry> mSceneEntryList = new ArrayList<TreeEntry>();
    //Tree entry for scenes
    private final TreeEntry mSceneFlowEntry = new TreeEntry("Scenes", null, null);
    //Tree entry for functions
    private final TreeEntry mFunctionsEntry = new TreeEntry("Functions", null, null);
    //Tree entry for DialogActs
    private final TreeEntry mDialogActsEntry = new TreeEntry("Dialog Acts", null, null);
    
    //Global element tree
    private final TreeEntry mElementTree = new TreeEntry("SceneFlow", Preferences.ICON_ROOT_FOLDER, null);
    
    
    //Popup menu buttons //TODO: does it work?
    private final JMenuItem functionsAdd = new JMenuItem("Add...");
    private final JMenuItem functionModify = new JMenuItem("Modify...");
    private final JMenuItem functionRemove = new JMenuItem("Remove");

    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();

    private final EditorProject mProject;
    private final DialogActInterface mDialogAct;
    // The sceneflow of the project
    private final SceneFlow mSceneFlow;

    // Drag & Drop support
    private DragSource mDragSource;
    private DragGestureListener mDragGestureListener;
    private DragSourceListener mDragSourceListener;
    private int mAcceptableDnDActions;

    public void updateFunctions() {
        mFunctionsEntry.removeAllChildren();

        List<FunDef> functionDefinitions = new ArrayList<FunDef>(mSceneFlow.getUsrCmdDefMap().values());

        Collections.sort(functionDefinitions);

        for (final FunDef def : functionDefinitions) {
            mFunctionsEntry.add(new TreeEntry(def.getName(), def.isValidClass()
                    ? Preferences.ICON_FUNCTION_ENTRY
                    : Preferences.ICON_FUNCTION_ERROR_ENTRY, def));
        }
    }
    /**
     *
     *
     */
    public ElementTree(EditorProject project) {
        super(new DefaultTreeModel(null));

        //
        mSceneFlow = project.getSceneFlow();
        //mSceneScriptEditor = scriptEditor;
        mProject = project;
        mDialogAct = mProject.getDialogAct();
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
        //expandAll();
        
    }

    //
    @Override
    public void valueChanged(TreeSelectionEvent e) {
        TreePath path = e.getPath();
        int pathCount = path.getPathCount();

        // Make sure that focus can be requested
        if (requestFocusInWindow()) {
            updateUI();
        }

        TreeEntry lastPathComponent = (TreeEntry) path.getLastPathComponent();

        if (pathCount == 3) {
            TreePath parentPath = path.getParentPath();

            if (parentPath.getLastPathComponent().equals(mFunctionsEntry)) {
                //mScriptEditorPanel.getTabPane().setSelectedIndex(1);
                // TODO: This is total bullshit cyclic dependencies and crossing the hiererchy
                // Realize that with the update event mechanism!!!!!
                //mScriptEditorPanel.getmParentPE().showSceneDocEditor();

                FunDef selectedDef = (FunDef) ((TreeEntry) path.getLastPathComponent()).getData();

                launchFunctionSelectedEvent(selectedDef);
            } else if (parentPath.getLastPathComponent().equals(mDialogActsEntry)) {
                //mScriptEditorPanel.getTabPane().setSelectedIndex(2);

                DialogAct selectedDA = (DialogAct) ((TreeEntry) path.getLastPathComponent()).getData();

                launchDASelectedEvent(selectedDA);
            } else if (lastPathComponent.getData()!=null){
                
                if (lastPathComponent.getData().getClass().equals(de.dfki.vsm.model.scenescript.SceneGroup.class)) {
                //mScriptEditorPanel.getTabPane().setSelectedIndex(0);

                String sceneLanguageSelect = ((TreeEntry) parentPath.getLastPathComponent()).getText();
                SceneGroup selectedScene = (SceneGroup) ((TreeEntry) path.getLastPathComponent()).getData();

                launchSceneSelectedEvent(selectedScene, sceneLanguageSelect);
                }
            }
        }
    }

    /**
     *
     *
     */
    public void expandAll() {
        for (int i = 0; i < getRowCount(); i++) {
            expandRow(i);
        }

        updateUI();
    }
    
    public void expandAllRecursive(){
        TreeNode root = (TreeNode)this.getModel().getRoot();
        if (root!=null) {   
            // Traverse tree from root
            expandAll(new TreePath(root), "Dialog Acts");
        }
        
    }
    

    /**
     * @return Whether an expandPath was called for the last node in the parent path
     */
    private boolean expandAll(TreePath parent, String skip) {
        // Traverse children
        // Variable skip is to indicate that we dont want to expand certain node
        TreeNode node = (TreeNode)parent.getLastPathComponent();
        if(((TreeEntry)node).getText().equals(skip)){
            return false;
        }
        if (node.getChildCount() > 0) {
            boolean childExpandCalled = false;
            for (Enumeration e=node.children(); e.hasMoreElements(); ) {
                TreeNode n = (TreeNode)e.nextElement();
                TreePath path = parent.pathByAddingChild(n);
                childExpandCalled = expandAll(path, skip) || childExpandCalled; // the OR order is important here, don't let childExpand first. func calls will be optimized out !
            }

            if (!childExpandCalled) { // only if one of the children hasn't called already expand
                // Expansion or collapse must be done bottom-up, BUT only for non-leaf nodes
                
                this.expandPath(parent);
                
            }
            return true;
        } else {
            return false;
        }
    }

    /**
     *
     *
     */
    private void initComponents() {

        // Add basic element entries
        // COMMENTED BY M. FALLAS ON 03-2015

        /*        mBasicEntry.add(mSuperNodeEntry);
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
         mRootEntry.add(mSceneEntry);*/
//      TreeEntry mSceneEntry = new TreeEntry("Scenes", null, null);
//      mSceneListEntry.add(mSceneEntry);
//      for(int i = 0; i < mSceneListEntry.size(); i++) {
//          mRootEntry.add(mSceneListEntry.get(i));
//      }
        mElementTree.add(mSceneFlowEntry);
        mElementTree.add(mFunctionsEntry);
        mElementTree.add(mDialogActsEntry);
        
        //
        ((DefaultTreeModel) getModel()).setRoot(mElementTree);
        functionsAdd.addActionListener(this);
        functionModify.addActionListener(this);
        functionRemove.addActionListener(this);
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        addTreeSelectionListener(this);
        refresh();
    }
    /**
     * Refresh the element tree 
     */
    public final void refresh() {
        // Print some information
        mLogger.message("Refreshing '" + this + "'");
        updateSceneList();
        updateFunctions();
        updateDialogActs();
        //remove empty scenes
        for(int i = 0; i < mSceneFlowEntry.getChildCount(); i++)
        {
            TreeEntry tempTE = (TreeEntry)mSceneFlowEntry.getChildAt(i);
            if(tempTE.getChildCount() ==0)
            {
                mSceneEntryList.remove(tempTE);
                mSceneFlowEntry.remove(i);
            }
        }
        // Update the visual appearance of the ElementTree
        updateUI();
    }
    /**
     * Refresh the list of scenes
     *
     */
    private void updateSceneList() {
        // System.out.println("Updating Scenes");
        for (int i = 0; i < mSceneEntryList.size(); i++) {
            mSceneEntryList.get(i).removeAllChildren();

            if (mSceneEntryList.get(i).isNodeChild(mSceneFlowEntry)) {

                mSceneFlowEntry.remove(mSceneEntryList.get(i));
            }
        }
        
        //
        SceneScript sceneScript = mProject.getSceneScript();

        if (sceneScript != null) {
            if (sceneScript.getSceneListSize() > 0) {
                for (SceneGroup group : sceneScript.getOrderedGroupSet().descendingSet()) {
                    ArrayList<SceneObject> whiteList = group.getWhiteList();
                    ArrayList<SceneObject> blackList = group.getBlackList();
                    ArrayList<String> languageList = new ArrayList<String>();

//                    if (mSceneFlowEntry.isNodeDescendant(mSceneListEntry)) {
//                        mSceneFlowEntry.remove(mSceneListEntry);
//                    }

                    for (SceneObject scene : whiteList) {

                         //System.out.println("White List - Name: " + scene.getName() + "Language: " + scene.getLanguage());
                        
                        if (!languageList.contains(scene.getLanguage())) {
                            languageList.add(scene.getLanguage());
                            
                            TreeEntry sceneEntry = getSceneEntry(scene.getLanguage());
                            // Add new scene language to the root entry
                            if (sceneEntry != null) {
                                sceneEntry.add(new TreeEntry(scene.getName(),  null, group));    // PG: added a space before the number cnt of scenes in scenegroup for better readability
                            } else {
                                sceneEntry = new TreeEntry("Scenes (" + scene.getLanguage() + ")", null, null);
                                sceneEntry.add(new TreeEntry(scene.getName(), null, group));
                                if (sceneEntry.getChildCount() >0 )
                                {
                                    mSceneEntryList.add(sceneEntry);
                                    mSceneFlowEntry.add(sceneEntry);
                                }
                            }
                        }
                    }

                    for (SceneObject scene : blackList) {

                        // System.out.println("Black List - Name: " + scene.getName() + "Language: " + scene.getLanguage());
                        if (!languageList.contains(scene.getLanguage())) {
                            languageList.add(scene.getLanguage());

                            TreeEntry sceneEntry = getSceneEntry(scene.getLanguage());

                            // Add new scene language to the root entry
                            if (sceneEntry == null) {
                                sceneEntry = new TreeEntry("Scenes (" + scene.getLanguage() + ")", null, null);
                                sceneEntry.add(new TreeEntry(scene.getName(), null, group));
                                mSceneEntryList.add(sceneEntry);
                                mSceneFlowEntry.add(sceneEntry);
                            } else {
                                sceneEntry.add(new TreeEntry(scene.getName(), null, group));    // PG: added a space before the number cnt of scenes in scenegroup for better readability
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Refresh the list of dialog acts
     */
    void updateDialogActs() {
        mDialogActsEntry.removeAllChildren();

        Map<String, List<String>> attributeValueMap = new HashMap();
        List<String> valueList = new ArrayList<>();

        // Populate attributeValueMap with DA attributes and its values
        for (String att : mProject.getDialogAct().getNLGAttributes()) {
            for (String val : mProject.getDialogAct().getNLGAttributeValues(att)) {
                valueList.add(val);
            }

            attributeValueMap.put(att, valueList);
        }

        for (String phase : mProject.getDialogAct().getDialogueActPhases()) {
            for (String da : mProject.getDialogAct().getDialogueActs(phase)) {
                mDialogActsEntry.add(new TreeEntry(da + " (" + phase + ")", null, new DialogAct(da, phase, attributeValueMap)));
            }
        }
    }

   
    /**
     * Control of clicks for the popup menu
     * @param tree
     * @return 
     */
    public MouseAdapter getMouseAdapter(final JTree tree) {
        return new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                TreePath path = tree.getPathForLocation(e.getX(), e.getY());

                if (path == null) {
                    return;
                }

                tree.setSelectionPath(path);

                boolean showPopup = false;
                JPopupMenu menu = new JPopupMenu();
                int pathCount = path.getPathCount();

                // System.out.println(pathCount);
                // TODO: why do we check the pathCount?
                // test if the user clicked on the Functions entry
                if (pathCount == 2) {
                    if (path.getLastPathComponent().equals(mFunctionsEntry)) {
                        if (e.isPopupTrigger()) {
                            menu.add(functionsAdd);
                            showPopup = true;
                        }
                    }

                    if (path.getLastPathComponent() instanceof TreeEntry) {
                        launchTreeEntrySelectedEvent((TreeEntry) path.getLastPathComponent());
                    }
                } // test if the user clicked on exact function
                else if (pathCount == 3) {
                    TreePath parentPath = path.getParentPath();

                    if (parentPath.getLastPathComponent().equals(mFunctionsEntry)) {
                        FunDef selectedDef = (FunDef) ((TreeEntry) path.getLastPathComponent()).getData();

                        launchFunctionSelectedEvent(selectedDef);

                        if (e.isPopupTrigger()) {
                            menu.add(functionModify);
                            menu.add(functionRemove);
                            showPopup = true;
                        } else if ((e.getClickCount() == 2) && !e.isConsumed()) {
                            TreeEntry entry = (TreeEntry) path.getLastPathComponent();

                            modifyFunctionAction(entry);
                        }
                    } else if (parentPath.getLastPathComponent().equals(mDialogActsEntry)) {
                        if (e.isPopupTrigger()) {
                        } else if ((e.getClickCount() == 2) && !e.isConsumed()) {
                            TreeEntry entry = (TreeEntry) path.getLastPathComponent();

                            modifyDialogAct(entry);
                        }
                    }
                }

                if (showPopup) {
                    menu.show(tree, e.getX(), e.getY());
                }
            }

            private void launchFunctionSelectedEvent(FunDef funDef) {
                FunctionSelectedEvent ev = new FunctionSelectedEvent(this, funDef);

                mEventCaster.convey(ev);
            }

            private void launchTreeEntrySelectedEvent(TreeEntry entry) {
                TreeEntrySelectedEvent ev = new TreeEntrySelectedEvent(this, entry);

                mEventCaster.convey(ev);
            }

            private void modifyDialogAct(TreeEntry entry) {
                if (entry != null) {
                    DialogActAttributes daAttributeDialog = new DialogActAttributes(mDialogAct, entry.getText());

                    daAttributeDialog.run();
                }
            }
        };
    }

    /**
     *
     *
     */
    private final void modifyFunctionAction(final TreeEntry entry) {
        if (entry != null) {
            FunDef usrCmdDef = new FunDefDialog((FunDef) entry.getData()).run();

            if (usrCmdDef != null) {
                FunDef oldFunDef = (FunDef) entry.getData();

                mSceneFlow.removeUsrCmdDef(oldFunDef.getName());
                mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
                updateFunctions();

                FunctionModifiedEvent ev = new FunctionModifiedEvent(this, usrCmdDef);

                EventDispatcher.getInstance().convey(ev);
            }
        }
    }

   //
    private String getEntryName(final TreeEntry entry) {
        if (entry != null) {
            FunDef oldFunDef = (FunDef) entry.getData();

            return oldFunDef.getName();
        }

        return null;
    }
    /**
     * 
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();

        if (source == functionsAdd) {
            FunDef usrCmdDef = new FunDefDialog(null).run();

            if (usrCmdDef != null) {
                mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
                updateFunctions();
                EditorInstance.getInstance().refresh();
                launchFunctionCreatedEvent(usrCmdDef);
            }
        } else if (source == functionModify) {
            TreeEntry entry = (TreeEntry) getLastSelectedPathComponent();

            modifyFunctionAction(entry);
            EditorInstance.getInstance().refresh();
            launchFunctionSelectedEvent((FunDef) entry.getData());
        } else if (source == functionRemove) {
            TreeEntry entry = (TreeEntry) getLastSelectedPathComponent();

            if (entry != null) {
                FunDef oldFunDef = (FunDef) entry.getData();

                mSceneFlow.removeUsrCmdDef(oldFunDef.getName());
                updateFunctions();

                EditorInstance.getInstance().refresh();
                launchFunctionCreatedEvent((FunDef) entry.getData());
            }
        }
    }

    private void launchFunctionSelectedEvent(FunDef funDef) {
        FunctionSelectedEvent ev = new FunctionSelectedEvent(this, funDef);

        mEventCaster.convey(ev);
    }

    private void launchFunctionCreatedEvent(FunDef funDef) {
        FunctionCreatedEvent ev = new FunctionCreatedEvent(this, funDef);

        mEventCaster.convey(ev);
    }

    private void launchSceneSelectedEvent(SceneGroup sceneGroup, String sceneLanguage) {
        SceneSelectedEvent ev = new SceneSelectedEvent(this, sceneGroup);

        ev.setLanguage(sceneLanguage);
        mEventCaster.convey(ev);
    }

    private void launchDASelectedEvent(DialogAct dialogAct) {
        DialogActSelectedEvent ev = new DialogActSelectedEvent(this, dialogAct);

        mEventCaster.convey(ev);
    }

    /**
     * Drag and Drop support
     */
    private void initDnDSupport() {

        // Create the default drag source
        mDragSource = DragSource.getDefaultDragSource();

        // Install the drag source listener
        mDragSourceListener = new DragSourceAdapter() {
        };

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

    public boolean isSceneLanguageAlreadyExist(String language) {
        boolean isLang = false;

        for (int i = 0; i < mSceneEntryList.size(); i++) {
            if (mSceneEntryList.get(i).getText().equals("Scenes (" + language + ")")) {
                isLang = true;
            }
        }

        return isLang;
    }

    public TreeEntry getSceneEntry(String language) {
        for (int i = 0; i < mSceneEntryList.size(); i++) {

//          System.out.println("Compare: " + mSceneListEntry.get(i).getText() + 
//                  " with " + "Scenes (" + language + ")");
            if (mSceneEntryList.get(i).getText().equals("Scenes (" + language + ")")) {
                return mSceneEntryList.get(i);
            }
        }

        return null;
    }

    /**
     *
     *
     */
    private class CellRenderer extends DefaultTreeCellRenderer {

        public CellRenderer() {
            super();

            // setBackgroundNonSelectionColor(UIManager.getColor("Panel.background"));
        }

        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selection, boolean expanded,
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
}
