package de.dfki.vsm.editor;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.dialog.SceneActionDialog;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.acticon.ActiconAction;
import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.gesticon.GesticonAgent;
import de.dfki.vsm.model.gesticon.GesticonGesture;
import de.dfki.vsm.util.evt.EventDispatcher;
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
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;

/**
 * @author Gregor Mehlmannn
 */
public final class SceneElementDisplay extends JScrollPane implements EventListener {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventDispatcher mEventMulticaster = EventDispatcher.getInstance();

    private final SceneElementTree mSceneElementTree = new SceneElementTree();

    // The editor project of this editor
    private final EditorProject mProject;

    // Construct a scene element display 
    public SceneElementDisplay(final EditorProject project) {

        mProject = project;

        //
        setBackground(Color.WHITE);
        setPreferredSize(new Dimension( 250, 200));
        setBorder(BorderFactory.createEtchedBorder());
        setViewportView(mSceneElementTree);
    }

//    @Override
//    public void update(final java.util.Observable obs, final Object obj) {
//
//        // mLogger.message("SceneElementDisplay.update(" + obj + ")");
//        mObservable.update(obj);
//    }
    @Override
    public void update(final EventObject event) {
    }
//
//    private class Observable extends java.util.Observable {
//
//        public void update(Object obj) {
//            setChanged();
//            notifyObservers(obj);
//        }
//    }

    // Refresh the visual appearance
    public final void refresh() {
        mSceneElementTree.refresh();
    }
//}

    /**
     *
     *
     *
     *
     *
     */
//class Entry extends DefaultMutableTreeNode implements Transferable {
//
//    public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
//        return userObject;
//    }
//
//    public boolean isDataFlavorSupported(DataFlavor flavor) {
//        return true;
//    }
//
//    public DataFlavor[] getTransferDataFlavors() {
//        return null;
//    }
//
//    public Entry(Object userObject) {
//        super(userObject);
//    }
//}
    /**
     *
     *
     *
     *
     *
     */
    private class SceneElementTree extends JTree implements EventListener {

        // Elements
        private final TreeEntry mRootEntry = new TreeEntry("Scene Elements", Preferences.ICON_ROOT_FOLDER, null);
        private final TreeEntry mGesticonEntry = new TreeEntry("Gesticon", null, null);
        private final TreeEntry mActionDefinitionsEntry = new TreeEntry("Acticon", null, null);

        // Drag & drop support
        private DragSource mDragSource;
        private DragGestureListener mDragGestureListener;
        private DragSourceListener mDragSourceListener;
        private int mAcceptableDnDActions;

        // Refresh the visual appearance
        public final void refresh() {
            showGesticon();
            showSceneAction();
        }

        /**
         *
         */
        public SceneElementTree() {
            super(new DefaultTreeModel(null));

            //
            getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

            //
            setBorder(BorderFactory.createEmptyBorder());
            setCellRenderer(new CellRenderer());
            setBackground(Color.WHITE);
            setRootVisible(true);

            // setRowHeight(0);
            //
            initDnDSupport();
            initComponents();

            //
            //expandAll();

            //
//      addTreeSelectionListener(new TreeSelectionListener() {
//
//        public void valueChanged(TreeSelectionEvent e) {
//        }
//      });
            addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {

                    /*
                     *
                     * Action node = (Action) getLastSelectedPathComponent();
                     * if (e.getButton() == MouseEvent.BUTTON3) {
                     * if (node != null) {
                     * if (node.equals(mActionDefinitionsEntry)) {
                     * showAddActionMenu(e, node);
                     * }
                     * if (node.getData() instanceof SceneAction) {
                     * showEditActionMenu(e, node);
                     * }
                     * }
                     * }
                     */
                }
            });
        }

//        @Override
//        public void update(java.util.Observable obs, Object obj) {
//
//        // mLogger.message("SceneElementTree.update(" + obj + ")");
//            //if (obj instanceof EditorProject) {
//            //EditorProject p = (EditorProject) obj;
//          
//            //}
//        }
        /**
         *
         *
         *
         *
         *
         */
        @Override
        public void update(EventObject event) {
        }

        public void showAddActionMenu(MouseEvent e, ActiconAction entry) {
            JPopupMenu pop = new JPopupMenu();
            JMenuItem item = new JMenuItem("Add");

            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    SceneActionDialog.getInstance().prepareNewAction();
                    SceneActionDialog.getInstance().setVisible(true);
                }
            });
            pop.add(item);
            pop.show(this, e.getX(), e.getY());
        }

        public void showEditActionMenu(MouseEvent e, ActiconAction entry) {
            JPopupMenu pop = new JPopupMenu();
            JMenuItem item = new JMenuItem("Copy");

            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent event) {

                    // Entry node = (Entry) getLastSelectedPathComponent();
                    // SM3SceneAction aToCopy = (SM3SceneAction) node.getData();
                    // SM3SceneAction copyedAction = (SM3SceneAction) aToCopy.getCopy();
                    // copyedAction.setActionName(copyedAction.getActionName() + " (copy)");
                    // SM3SceneActicon asd = Editor.getInstance().getSelectedProjectEditor().getProject().getSceneActions();
                    // asd.addAction(copyedAction);
                    // showSceneAction(Editor.getInstance().getSelectedProjectEditor().getProject());
                }
            });
            pop.add(item);
            item = new JMenuItem("Edit");
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent event) {
                    TreeEntry node = (TreeEntry) getLastSelectedPathComponent();
                    int pos = 0;
                    ActiconAction a = (ActiconAction) node.getData();

                    // remove it
                    SceneActionDialog.getInstance().editAction(a, pos);
                    SceneActionDialog.getInstance().setVisible(true);
                    node.removeFromParent();

                    ActiconConfig asd = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getActicon();

                    asd.remove(a);
                }
            });
            pop.add(item);
            pop.addSeparator();
            item = new JMenuItem("Delete");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    TreeEntry node = (TreeEntry) getLastSelectedPathComponent();
                    ActiconAction aToDel = (ActiconAction) node.getData();

                    node.removeFromParent();

                    ActiconConfig asd = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getActicon();

                    asd.remove(aToDel);
                    // TODO: Why do we save the acticon here, that is bullshit
                    //Editor.getInstance().getSelectedProjectEditor().getProject().saveActicon();
                    //showSceneAction(Editor.getInstance().getSelectedProjectEditor().getProject());
                }
            });
            pop.add(item);
            pop.show(this, e.getX(), e.getY());
        }
        
        public void expandAll() {
            for (int i = 0; i < getRowCount(); i++) {
                expandRow(i);
            }

            updateUI();
        }
        private void initComponents() {
            mRootEntry.add(mGesticonEntry);
            mRootEntry.add(mActionDefinitionsEntry);

            //
            ((DefaultTreeModel) getModel()).setRoot(mRootEntry);
        }

        private void showGesticon(/*EditorProject project*/) {

            // Construct the tree from the gestion
            // Entry gesticonNode = new Entry("Gesticon", null, project.getGesticon());
            for (GesticonAgent characterEntry : mProject.getGesticon().getAgentList()) {
                TreeEntry characterNode = new TreeEntry("Character", null, characterEntry);

                for (GesticonGesture gestureEntry : characterEntry.getGestureList()) {
                    TreeEntry gestureNode = new TreeEntry("Gesture", null, gestureEntry);

                    characterNode.add(gestureNode);
                }

                mGesticonEntry.add(characterNode);
            }    // mGesticonEntry.add(gesticonNode);

            // ((DefaultTreeModel) getModel()).setRoot(gesticonNode);
            // Expand the gesticon tree
            //expandAll();
        }

        private void showSceneAction(/*EditorProject p*/) {

            /*
             * mActionDefinitionsEntry.removeAllChildren();
             * for (ScriptAction a : p.getSceneActions().getEntryList()) {
             * Entry actionNode = new Entry(a.getText(), null, a);
             *
             * mActionDefinitionsEntry.add(actionNode);
             * }
             * expandAll();
             */
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
                    TreeEntry selectedEntry = (TreeEntry) getSelectionPath().getLastPathComponent();

                    mDragSource.startDrag(event, DragSource.DefaultCopyDrop, selectedEntry, mDragSourceListener);
                }
            };

            // Set the acceptable actions
            mAcceptableDnDActions = DnDConstants.ACTION_COPY;

            // Set the default drag gesture recognizer
            mDragSource.createDefaultDragGestureRecognizer(this, mAcceptableDnDActions, mDragGestureListener);
        }

        /**
         *
         *
         *
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

                // Get the user object
                // Object userObject = ((Entry) value).getUserObject();
                // Render the cell
//          if (data instanceof GesticonEntry) {
//              //setIcon(null);
//              setText(((GesticonEntry) data).getGestureId());
//              setToolTipText(((GesticonEntry) data).getGestureId());
//          } else if (data instanceof CharacterEntry) {
//              setIcon(new ImageIcon(((CharacterEntry) data).getCharacterIcon()));
//              setText(((CharacterEntry) data).getCharacterName());
//              setToolTipText(((CharacterEntry) data).getCharacterName());
//          } else if (data instanceof Gesticon) {
//              setText("Gesticon");
//              setToolTipText("Gesticon");
//          }
                // render the cell
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
}
