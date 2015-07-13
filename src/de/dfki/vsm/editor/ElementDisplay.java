package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.instance.EditorInstance;
import de.dfki.vsm.editor.dialog.DialogActAttributes;
import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.event.DialogActSelectedEvent;
import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.FunctionModifiedEvent;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.editor.event.SceneSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.editor.project.auxiliary.AuxiliaryEditor;
import de.dfki.vsm.model.dialogact.DialogAct;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import static de.dfki.vsm.editor.util.Preferences.sFUNCTION_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sFUNCTION_ERROR_ENTRY;
import static de.dfki.vsm.editor.util.Preferences.sROOT_FOLDER;

//~--- JDK imports ------------------------------------------------------------

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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observer;

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
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

/**
 *
 *
 */
public class ElementDisplay extends JScrollPane implements Observer, EventListener {
    private final Observable        mObservable       = new Observable();
    private final LOGDefaultLogger  mLogger           = LOGDefaultLogger.getInstance();
    private final EventDispatcher       mEventMulticaster = EventDispatcher.getInstance();
    private final ElementTree       mElementTree;
    //private final SceneScriptEditor mSceneScriptEditor;

    public ElementDisplay(SceneFlow sceneFlow, EditorProject project/*, SceneScriptEditor scriptEditor*/) {
       // mSceneScriptEditor = scriptEditor;
        mElementTree  = new ElementTree(sceneFlow, project/*, mSceneScriptEditor*/);
        mObservable.addObserver(mElementTree);

        //
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEtchedBorder());
        setPreferredSize(new Dimension(250, 200));
        setViewportView(mElementTree);
    }

    @Override
    public void update(java.util.Observable obs, Object obj) {

        // mLogger.message("ElementDisplay.update(" + obj + ")");
        mObservable.update(obj);
        updateFunctionList();
        updateDAList();
    }

    private void updateFunctionList() {
        mElementTree.updateFunDefs();
    }

    private void updateDAList() {
        mElementTree.updatDialogueActs();
    }

    @Override
    public void update(EventObject event) {}

    public ElementTree getElementTree() {
        return mElementTree;
    }

    private class Observable extends java.util.Observable {
        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
}


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
 *
 *
 */
class ElementTree extends JTree implements Observer, EventListener, ActionListener, TreeSelectionListener {
    private final TreeEntry mRootEntry = new TreeEntry("SceneFlow", sROOT_FOLDER, null);

    // private final TreeEntry mBasicEntry = new TreeEntry("Elements", null, null);
    private final TreeEntry      mSceneEntry     = new TreeEntry("Scenes", null, null);
    private ArrayList<TreeEntry> mSceneListEntry = new ArrayList<TreeEntry>();
    private final TreeEntry      mFunDefEntry    = new TreeEntry("Functions", null, null);
    private final TreeEntry      mDAEntry        = new TreeEntry("DialogActs", null, null);

    // COMMENTED BY M. FALLAS ON 03-2015

    /*
     * private final TreeEntry mSuperNodeEntry = new TreeEntry("Super Node", sSUPERNODE_ENTRY, Node.Type.SuperNode);
     * private final TreeEntry mBasicNodeEntry = new TreeEntry("Basic Node", sBASICNODE_ENTRY, Node.Type.BasicNode);
     * private final TreeEntry mEEdgeEntry = new TreeEntry("Epsilon Edge", sEEDGE_ENTRY, new Edge(Edge.TYPE.EEDGE));
     * private final TreeEntry mTEdgeEntry = new TreeEntry("Timeout Edge", sTEDGE_ENTRY, new Edge(Edge.TYPE.TEDGE));
     * private final TreeEntry mPEdgeEntry = new TreeEntry("Probability Edge", sPEDGE_ENTRY, new Edge(Edge.TYPE.PEDGE));
     * private final TreeEntry mCEdgeEntry = new TreeEntry("Conditional Edge", sCEDGE_ENTRY, new Edge(Edge.TYPE.CEDGE));
     * private final TreeEntry mIEdgeEntry = new TreeEntry("Interruptive Edge", sIEDGE_ENTRY, new Edge(Edge.TYPE.IEDGE));
     * private final TreeEntry mFEdgeEntry = new TreeEntry("Fork Edge", sFEDGE_ENTRY, new Edge(Edge.TYPE.FEDGE));
     * private final TreeEntry mCommentEntry = new TreeEntry("Comment", sCOMMENT_ENTRY, new Comment());
     */
    private final JMenuItem functionsAdd   = new JMenuItem("Add...");
    private final JMenuItem functionModify = new JMenuItem("Modify...");
    private final JMenuItem functionRemove = new JMenuItem("Remove");

    //
    private final LOGDefaultLogger mLogger      = LOGDefaultLogger.getInstance();
    private final EventDispatcher      mEventCaster = EventDispatcher.getInstance();

    // private final EventCaster mEventMulticaster = EventCaster.getInstance();
    // private final Observable mObservable = new Observable();
    private final SceneFlow         mSceneFlow;
    //private final SceneScriptEditor mSceneScriptEditor;

    // Drag & Drop support
    private DragSource               mDragSource;
    private DragGestureListener      mDragGestureListener;
    private DragSourceListener       mDragSourceListener;
    private int                      mAcceptableDnDActions;
    private final EditorProject        mProject;
    private final DialogActInterface mDialogAct;

    /**
     *
     *
     */
    public ElementTree(SceneFlow sceneFlow, EditorProject project/*, SceneScriptEditor scriptEditor*/) {
        super(new DefaultTreeModel(null));

        //
        mSceneFlow         = sceneFlow;
        //mSceneScriptEditor = scriptEditor;
        mProject           = project;
        mDialogAct         = mProject.getDialogAct();
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

    /*
     *
     *
     */

//  public class Observable extends java.util.Observable {
//
//      public void update(Object obj) {
//          setChanged();
//          notifyObservers(obj);
//      }
//  }
    @Override
    public void update(java.util.Observable obs, Object obj) {

        // mLogger.message("ElementTree.update(" + obj + ")");
        
        // TODO: We already have a reference to the project
        // Why do we need a reference in the update method
        if (obj instanceof EditorProject) {
            updateScenes((EditorProject) obj);
            updateFunDefs((EditorProject) obj);
        }

        // Update the visual appearance of the ElementTree
        updateUI();
    }

    /**
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

    @Override
    public void valueChanged(TreeSelectionEvent e) {
        TreePath path      = e.getPath();
        int      pathCount = path.getPathCount();

        // Make sure that focus can be requested
        if (requestFocusInWindow()) {
            updateUI();
        }

        TreeEntry lastPathComponent = (TreeEntry) path.getLastPathComponent();

        if (pathCount == 3) {
            TreePath parentPath = path.getParentPath();

            if (parentPath.getLastPathComponent().equals(mFunDefEntry)) {
                //mScriptEditorPanel.getTabPane().setSelectedIndex(1);
                // TODO: This is total bullshit cyclic dependencies and crossing the hiererchy
                // Realize that with the update event mechanism!!!!!
                //mScriptEditorPanel.getmParentPE().showSceneDocEditor();

                FunDef selectedDef = (FunDef) ((TreeEntry) path.getLastPathComponent()).getData();

                launchFunctionSelectedEvent(selectedDef);
            } else if (parentPath.getLastPathComponent().equals(mDAEntry)) {
                //mScriptEditorPanel.getTabPane().setSelectedIndex(2);

                DialogAct selectedDA = (DialogAct) ((TreeEntry) path.getLastPathComponent()).getData();

                launchDASelectedEvent(selectedDA);
            } else if (lastPathComponent.getData().getClass().equals(de.dfki.vsm.model.scenescript.SceneGroup.class)) {
                //mScriptEditorPanel.getTabPane().setSelectedIndex(0);

                String     sceneLanguageSelect = ((TreeEntry) parentPath.getLastPathComponent()).getText();
                SceneGroup selectedScene       = (SceneGroup) ((TreeEntry) path.getLastPathComponent()).getData();

                launchSceneSelectedEvent(selectedScene, sceneLanguageSelect);
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
        mRootEntry.add(mFunDefEntry);
        mRootEntry.add(mDAEntry);

        //
        ((DefaultTreeModel) getModel()).setRoot(mRootEntry);
        functionsAdd.addActionListener(this);
        functionModify.addActionListener(this);
        functionRemove.addActionListener(this);
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        addTreeSelectionListener(this);
    }

    /**
     *
     *
     */
    private void updateScenes(EditorProject project) {

        //
        // System.out.println("Updating Scenes");
        for (int i = 0; i < mSceneListEntry.size(); i++) {
            mSceneListEntry.get(i).removeAllChildren();

            // System.out.println("Getting:" + mSceneListEntry.get(i).getText());
            if (mSceneListEntry.get(i).isNodeChild(mRootEntry)) {

                // System.out.println("Removing: " + mSceneListEntry.get(i).getText());
                mRootEntry.remove(mSceneListEntry.get(i));
            }
        }

        //
        SceneScript sceneScript = project.getSceneScript();

        if (sceneScript != null) {
            if (sceneScript.getSceneListSize() > 0) {
                for (SceneGroup group : sceneScript.getOrderedGroupSet().descendingSet()) {
                    ArrayList<SceneObject> whiteList    = group.getWhiteList();
                    ArrayList<SceneObject> blackList    = group.getBlackList();
                    ArrayList<String>      languageList = new ArrayList<String>();

                    if (mRootEntry.isNodeDescendant(mSceneEntry)) {
                        mRootEntry.remove(mSceneEntry);
                    }

                    for (SceneObject scene : whiteList) {

                        // System.out.println("White List - Name: " + scene.getName() + "Language: " + scene.getLanguage());
                        if (!languageList.contains(scene.getLanguage())) {
                            languageList.add(scene.getLanguage());

                            TreeEntry sceneEntry = getSceneEntry(scene.getLanguage());

                            // Add new scene language to the root entry
                            if (sceneEntry == null) {
                                sceneEntry = new TreeEntry("Scenes (" + scene.getLanguage() + ")", null, null);
                                sceneEntry.add(new TreeEntry(scene.getName(), null, group));
                                mSceneListEntry.add(sceneEntry);
                                mRootEntry.add(sceneEntry);
                            } else {
                                sceneEntry.add(new TreeEntry(scene.getName(), null, group));    // PG: added a space before the number cnt of scenes in scenegroup for better readability
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
                                mSceneListEntry.add(sceneEntry);
                                mRootEntry.add(sceneEntry);
                            } else {
                                sceneEntry.add(new TreeEntry(scene.getName(), null, group));    // PG: added a space before the number cnt of scenes in scenegroup for better readability
                            }
                        }
                    }
                }
            }
        }

        //
        expandAll();
    }

    /**
     *
     */
    void updatDialogueActs() {
        mDAEntry.removeAllChildren();

        Map<String, List<String>> attributeValueMap = new HashMap();
        List<String>              valueList         = new ArrayList<>();

        // Populate attributeValueMap with DA attributes and its values
        for (String att : mProject.getDialogAct().getNLGAttributes()) {
            for (String val : mProject.getDialogAct().getNLGAttributeValues(att)) {
                valueList.add(val);
            }

            attributeValueMap.put(att, valueList);
        }

        for (String phase : mProject.getDialogAct().getDialogueActPhases()) {
            for (String da : mProject.getDialogAct().getDialogueActs(phase)) {
                mDAEntry.add(new TreeEntry(da + " (" + phase + ")", null, new DialogAct(da, phase, attributeValueMap)));
            }
        }
    }

    /**
     *
     */
    private void updatDialogueActs(EditorProject project) {
        mDAEntry.removeAllChildren();

        for (String phase : project.getDialogAct().getDialogueActPhases()) {

            // mDAEntry.add(new TreeEntry(phase, null, null));
            for (String da : project.getDialogAct().getDialogueActs(phase)) {

                // mDAEntry.add(new TreeEntry(da, null, new DialogAct(da, phase)));
            }
        }

        expandAll();
    }

    /**
     *
     *
     */
    public MouseAdapter getMouseAdapter(final JTree tree) {
        return new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                TreePath path = tree.getPathForLocation(e.getX(), e.getY());

                if (path == null) {
                    return;
                }

                tree.setSelectionPath(path);

                boolean    showPopup = false;
                JPopupMenu menu      = new JPopupMenu();
                int        pathCount = path.getPathCount();

                // System.out.println(pathCount);
                // TODO: why do we check the pathCount?
                // test if the user clicked on the Functions entry
                if (pathCount == 2) {
                    if (path.getLastPathComponent().equals(mFunDefEntry)) {
                        if (e.isPopupTrigger()) {
                            menu.add(functionsAdd);
                            showPopup = true;
                        }
                    }

                    if (path.getLastPathComponent() instanceof TreeEntry) {
                        launchTreeEntrySelectedEvent((TreeEntry) path.getLastPathComponent());
                    }
                }

                // test if the user clicked on exact function
                else if (pathCount == 3) {
                    TreePath parentPath = path.getParentPath();

                    if (parentPath.getLastPathComponent().equals(mFunDefEntry)) {
                        FunDef selectedDef = (FunDef) ((TreeEntry) path.getLastPathComponent()).getData();

                        launchFunctionSelectedEvent(selectedDef);

                        if (e.isPopupTrigger()) {
                            menu.add(functionModify);
                            menu.add(functionRemove);
                            showPopup = true;
                        } else if ((e.getClickCount() == 2) &&!e.isConsumed()) {
                            TreeEntry entry = (TreeEntry) path.getLastPathComponent();

                            modifyFunctionAction(entry);
                        }
                    } else if (parentPath.getLastPathComponent().equals(mDAEntry)) {
                        if (e.isPopupTrigger()) {}
                        else if ((e.getClickCount() == 2) &&!e.isConsumed()) {
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
                updateFunDefs();

                FunctionModifiedEvent ev = new FunctionModifiedEvent(this, usrCmdDef);

                EventDispatcher.getInstance().convey(ev);
            }
        }
    }

    /**
     *
     *
     */
    private void updateFunDefs(EditorProject project) {

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
     *
     *
     */
    private String getEntryName(final TreeEntry entry) {
        if (entry != null) {
            FunDef oldFunDef = (FunDef) entry.getData();

            return oldFunDef.getName();
        }

        return null;
    }

    /**
     *
     *
     */
    public void updateFunDefs() {
        mFunDefEntry.removeAllChildren();

        List<FunDef> functionDefinitions = new ArrayList<FunDef>(mSceneFlow.getUsrCmdDefMap().values());

        Collections.sort(functionDefinitions);

        for (final FunDef def : functionDefinitions) {
            mFunDefEntry.add(new TreeEntry(def.getName(), def.isValidClass()
                    ? sFUNCTION_ENTRY
                    : sFUNCTION_ERROR_ENTRY, def));
        }
    }

    /**
     *
     *
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();

        if (source == functionsAdd) {
            FunDef usrCmdDef = new FunDefDialog(null).run();

            if (usrCmdDef != null) {
                mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
                updateFunDefs();
                EditorInstance.getInstance().update();
                launchFunctionCreatedEvent(usrCmdDef);
            }
        } else if (source == functionModify) {
            TreeEntry entry = (TreeEntry) getLastSelectedPathComponent();

            modifyFunctionAction(entry);
            EditorInstance.getInstance().update();
            launchFunctionSelectedEvent((FunDef) entry.getData());
        } else if (source == functionRemove) {
            TreeEntry entry = (TreeEntry) getLastSelectedPathComponent();

            if (entry != null) {
                FunDef oldFunDef = (FunDef) entry.getData();

                mSceneFlow.removeUsrCmdDef(oldFunDef.getName());
                updateFunDefs();
                EditorInstance.getInstance().update();
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
     *
     *
     */
    private void initDnDSupport() {

        // Create the default drag source
        mDragSource = DragSource.getDefaultDragSource();

        // Install the drag source listener
        mDragSourceListener = new DragSourceAdapter() {}
        ;

        // Install the drag gesture listener
        mDragGestureListener = new DragGestureListener() {
            @Override
            public void dragGestureRecognized(DragGestureEvent event) {

                // TODO: NULLPOINTEREXCEPTION abfangen
                System.out.println("xxxxxxxxxxxxxxxxxxxxxxxxxx");

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

        for (int i = 0; i < mSceneListEntry.size(); i++) {
            if (mSceneListEntry.get(i).getText().equals("Scenes (" + language + ")")) {
                isLang = true;
            }
        }

        return isLang;
    }

    public TreeEntry getSceneEntry(String language) {
        for (int i = 0; i < mSceneListEntry.size(); i++) {

//          System.out.println("Compare: " + mSceneListEntry.get(i).getText() + 
//                  " with " + "Scenes (" + language + ")");
            if (mSceneListEntry.get(i).getText().equals("Scenes (" + language + ")")) {
                return mSceneListEntry.get(i);
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
            Icon   icon = ((TreeEntry) value).getIcon();

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
