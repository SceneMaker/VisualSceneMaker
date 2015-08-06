package de.dfki.vsm.editor.project.sceneflow;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.event.ProjectChangedEvent;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.datatransfer.Clipboard;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.LinkedList;
import java.util.Observable;
import java.util.Observer;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import javax.swing.TransferHandler;
import javax.swing.plaf.basic.BasicButtonUI;

/**
 * @author Gregor Mehlmann
 */
public class SceneFlowToolBar extends JToolBar implements Observer, EventListener  {

    private final ImageIcon ICON_PLAY_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play.png");
    private final ImageIcon ICON_PLAY_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play_blue.png");
    private final ImageIcon ICON_PAUSE_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/pause.png");
    private final ImageIcon ICON_PAUSE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/pause_blue.png");

    private final ImageIcon ICON_MORE_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png");
    private final ImageIcon ICON_MORE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png");

    private final ImageIcon ICON_LESS_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png");
    private final ImageIcon ICON_LESS_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png");

// The singelton logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The singelton runtime instance
    private final RunTimeInstance mRunTime = RunTimeInstance.getInstance();
    // The singelton editor instance
    private final EditorInstance mEditorInstance = EditorInstance.getInstance();
    // The singelton system clipboard
    private final Clipboard mSystemClipBoard = getToolkit().getSystemClipboard();
    // The parent sceneflow editor
    private final SceneFlowEditor mSceneFlowEditor;
    // The supernodes of the path display
    private final LinkedList<SuperNode> mPathComponents = new LinkedList<>();

    // The current editor project
    private final EditorProject mEditorProject;
    // The current editor config    
    private final EditorConfig mEditorConfig;

    // The button GUI components
    private JButton mElementButton;
    private JButton mModifyButton;
    private JButton mPlayButton;
    private JButton mStopButton;
    private JButton mShowVarButton;
    private JButton mStraighten;
    private JButton mNormalize;
    private JButton mSaveProject;
    private JButton mUndo;
    private JButton mRedo;

    // Path Display GUI Components
    private JPanel      mPathDisplay;
    private JScrollBar  mPathScrollBar;
    private JScrollPane mPathScrollPane;

    Action undoAction = UndoAction.getInstance();
    Action redoAction = RedoAction.getInstance();
    
    // TODO: why is this here?
    // It is here to simplify code in the zooming in/out operations
    private int mNodeSize;    
    
    // Construct a sceneflow editor toolbar
    public SceneFlowToolBar(
            final SceneFlowEditor editor,
            final EditorProject project) {
        // Create a horizontal toolbar
        super("SceneFlowToolBar", JToolBar.HORIZONTAL);
        // Initialize the sceneflow editor
        mSceneFlowEditor = editor;
        // Initialize the editor project
        mEditorProject = project;
        // Initialize the editor config
        mEditorConfig = mEditorProject.getEditorConfig();
        // Initialize the GUI components
        setRollover(true);
        setFloatable(false);
        setBorder(BorderFactory.createEmptyBorder(4, 0, 4, 0));
        initPreferences();
        initComponents();
        // Add the sceneflowtoolbar to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    @Override
    public void update(Observable obs, Object obj) {
        //System.out.println("entra update 1");
        initPreferences();
        checkChangesOnProject();
        
    }
    
    @Override
    public void update(EventObject event) {
        //System.out.println("entra update 2");
        checkChangesOnProject();
    }

    private void checkChangesOnProject() {
        if(mEditorProject.hasChanged())
        {
            mSaveProject.setEnabled(true);
        }
        checkRedoUndo();
    }
    private void checkRedoUndo()
    {
        if(undoAction.isEnabled())
        {
            mUndo.setEnabled(true);
        }
        if(!undoAction.isEnabled())
        {
            mUndo.setEnabled(false);
        }
        if(redoAction.isEnabled())
        {
            mRedo.setEnabled(true);
        }
        if(!redoAction.isEnabled())
        {
            mRedo.setEnabled(false);
        }
    }
   
    private void initPreferences() {
        if (mEditorInstance.getSelectedProjectEditor() != null) {
            for (Object keyObj : mEditorConfig.getKeySet()) {
                String key = (String) keyObj;

                if (key.equals("node_width")) {
                    mNodeSize = Integer.valueOf(mEditorConfig.getProperty(key));
                }
            }
        } else {
            for (Object keyObj : Preferences.getKeySet()) {
                String key = (String) keyObj;

                if (key.equals("node_width")) {
                    mNodeSize = Integer.valueOf(Preferences.getProperty(key));
                }
            }
        }
    }

    private void saveEditorConfig() {
        mEditorConfig.setProperty("node_width", Integer.toString(mNodeSize));
        mEditorConfig.setProperty("node_height", Integer.toString(mNodeSize));
        
        mEditorConfig.save(mEditorInstance.getSelectedProjectEditor().getEditorProject().getProjectFile());

        EditorInstance.getInstance().refresh();
    }

    //TODO: adding not explicit but via refresh method
    public void addPathComponent(SuperNode supernode) {
        mPathComponents.addLast(supernode);
        refreshDisplay();

        int va = mPathScrollBar.getMaximum();

        mPathScrollBar.setValue(va);
    }

    public SuperNode removePathComponent() {
        SuperNode sn = mPathComponents.removeLast();

        // String str = mPathComponents.removeLast();
        refreshDisplay();

        return sn;
    }

    public void setPathComponent(int index, SuperNode supernode) {
        mPathComponents.set(index, supernode);
        refreshDisplay();
    }

    private void sanitizeTinyButton(JButton b) {
        Dimension bDim = new Dimension(40, 40);

        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);
        b.setFocusable(false);
        b.setContentAreaFilled(false);
        b.setText(null);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

    private void sanitizeSmallButton(JButton b) {
        Dimension bDim = new Dimension(50, 40);

        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);
        b.setFocusable(false);
        b.setContentAreaFilled(false);
        b.setText(null);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

    private JSeparator createSeparator()
    {
        JSeparator js = new JSeparator(SwingConstants.VERTICAL);
        js.setPreferredSize(new Dimension(10, 30));
        js.setMinimumSize(new Dimension(10, 30));
        js.setMaximumSize(new Dimension(10, 30));
        return js;
    }
    /**
     *
     */
    private void initComponents() {

        //menu separator
        
        // 3 Layout sections in toolbar
        // | Element Space |  Sceneflow Space | Property Space
        //
        // Element space
        add(Box.createHorizontalStrut(2));
        /**
         * LESS AND MORE BUTTONS ARE INVERTED TO MATCH WITH THE LEFT SIZE
         */
        mElementButton = add(new AbstractAction("ACTION_SHOW_ELEMENTS",
                Boolean.valueOf(Preferences.getProperty("showelements"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png")) {
                    public void actionPerformed(ActionEvent evt) {
                        mSceneFlowEditor.showElementDisplay();

                        //
                        refreshButtons();
                        //changeElementButtonState();
                        //revalidate();
                        //repaint();
                    }
                });
        mElementButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelements"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
        sanitizeTinyButton(mElementButton);
        add(Box.createHorizontalStrut(30));

        
        //******************************************************************************************************
        //EDIT PROJECT SECTION
        //Save project
        mSaveProject = add(new AbstractAction("ACTION_SAVEPROJECT", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/save_icon.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.save();
                mSaveProject.setEnabled(false);
            }
        });
        mSaveProject.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/save_icon_blue.png"));
        mSaveProject.setDisabledIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/save_icon_disable.png"));
        mSaveProject.setToolTipText("Save current project");
        sanitizeTinyButton(mSaveProject);
        mSaveProject.setEnabled(false);
        
        //Undo last action
        mUndo = add(new AbstractAction("ACTION_UNDO", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/undo_icon.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                undoAction.actionPerformed(e);
                checkRedoUndo();
            }
        });
        mUndo.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/undo_icon_blue.png"));
        mUndo.setDisabledIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/undo_icon_disabled.png"));
        mUndo.setToolTipText("Undo last action");
        sanitizeTinyButton(mUndo);
        mUndo.setEnabled(false);
        
        //Redo last action
        mRedo = add(new AbstractAction("ACTION_REDO", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/redo_icon.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                redoAction.actionPerformed(e);
                checkRedoUndo();
            }
        });
        mRedo.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/redo_icon_blue.png"));
        mRedo.setDisabledIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/redo_icon_disabled.png"));
        mRedo.setToolTipText("Redo last action");
        sanitizeTinyButton(mRedo);
        mRedo.setEnabled(false);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());
        //******************************************************************************************************
        //PROJECT EDITION SECTION 
        // Button to straighten all edeges
        mNormalize = add(new AbstractAction("ACTION_NORMALIZE", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/normalize_edges_gray.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().normalizeAllEdges();
            }
        });
        mNormalize.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/normalize_edges_blue.png"));
        mNormalize.setToolTipText("Normalize all edges");
        sanitizeTinyButton(mNormalize);
        // Button to straighten all edeges
        mStraighten = add(new AbstractAction("ACTION_STRAIGHTEN", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/straighten_gray.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().straightenAllEdges();
            }
        });
        mStraighten.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/straighten_blue.png"));
        mStraighten.setToolTipText("Straighten all edges");
        sanitizeTinyButton(mStraighten);
        // The Show Variables Button
        mShowVarButton = add(new AbstractAction("ACTION_SHOW_VARIABLES",
                Boolean.valueOf(Preferences.getProperty("showVariables"))
                        ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var.png")
                        : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden.png")) {
                    public void actionPerformed(ActionEvent evt) {
                        mSceneFlowEditor.getWorkSpace().showVariablesOnWorkspace();
                        changeShowVariablesButtonState();
                        revalidate();
                        repaint();
                    }
                });
        mShowVarButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showVariables"))
                                       ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_blue.png")
                                       : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden_blue.png"));
        mShowVarButton.setToolTipText(Boolean.valueOf(Preferences.getProperty("showVariables"))
                                      ? "Show Variables"
                                      : "Hide Variables");
        // Format The Button As Tiny
        sanitizeTinyButton(mShowVarButton);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());

        add(Box.createHorizontalStrut(30));
        addSeparator();
        add(Box.createHorizontalStrut(30));
        // The Play SceneFlow Button
        mPlayButton = add(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.play(mEditorProject);
            }
        });
        mPlayButton.setIcon(ICON_PLAY_STANDARD);
        mPlayButton.setRolloverIcon(ICON_PLAY_ROLLOVER);
        mPlayButton.setToolTipText("Start the execution of the sceneflow");
        sanitizeTinyButton(mPlayButton);
        // The Stop SceneFlow Button
        mStopButton = add(new AbstractAction("ACTION_STOP", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop.png")) {
            @Override
            public final void actionPerformed(ActionEvent e) {
                mEditorInstance.stop(mEditorProject);
            }
        });
        mStopButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop_blue.png"));
        mStopButton.setToolTipText("Stop Scene");
        // Format The Button As Tiny
        sanitizeTinyButton(mStopButton);

        JButton b = add(new AbstractAction("ACTION_WINDOW", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stack_icon.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                EditorInstance.getInstance().showMonitor();
            }
        });
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stack_icon_blue.png"));
        b.setToolTipText("Variable Manager");
        sanitizeTinyButton(b);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());

        //******************************************************************************************************
        // CONTROL OF NODES
        // Add Some Horizontal Space
        initPathDisplay();
        add(mPathScrollPane);
        //UP TO PARENT NODE 
        b = add(new AbstractAction("ACTION_LEVEL_UP", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mSceneFlowEditor.getWorkSpace().decreaseWorkSpaceLevel();
            }
        });
        b.setToolTipText("Up to parent node");
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up_blue.png"));
        sanitizeTinyButton(b);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());
        //******************************************************************************************************
        // SCREEN CONTROL
        //SCREENSHOT BUTTON
        Action action = new AbstractAction("ACTION_SCREEN_SHOT",
                ResourceLoader.loadImageIcon("/res/img/toolbar_icons/screenshot.png")) {
                    @Override
                    public void actionPerformed(ActionEvent evt) {
                        TransferHandler handler = mSceneFlowEditor.getWorkSpace().getTransferHandler();

                        if (handler != null) {
                            handler.exportToClipboard(mSceneFlowEditor.getWorkSpace(), mSystemClipBoard, TransferHandler.COPY);
                        } else {
                            System.err.println("handler null");
                        }
                    }
                };
        action.putValue(Action.SHORT_DESCRIPTION, "Add/Remove window");
        b = add(action);
        b.setToolTipText("Take a screenshot");
        //ZOOM OUT BUTTON
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/screenshot_blue.png"));
        sanitizeSmallButton(b);
        b = add(new AbstractAction("ACTION_ZOOM_IN", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomin.png")) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize < 190)
                        ? mNodeSize = mNodeSize + 10
                        : mNodeSize;
                saveEditorConfig();
            }
        });
        b.setToolTipText("Zoom In");
        //ZOOM IN BUTTON
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomin_blue.png"));
        sanitizeSmallButton(b);
        b = add(new AbstractAction("ACTION_ZOOM_OUT", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomout.png")) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize > 30)
                        ? mNodeSize = mNodeSize - 10
                        : mNodeSize;
                saveEditorConfig();
            }
        });
        b.setToolTipText("Zoom Out");
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomout_blue.png"));
        sanitizeTinyButton(b);

        //
        // Property Space
        //
        add(Box.createHorizontalGlue());
        mModifyButton = add(new AbstractAction("ACTION_SHOW_ELEMENTPROP",
                Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png")) {
                    public void actionPerformed(ActionEvent evt) {
                        mSceneFlowEditor.showElementEditor();
                        changeModifyButtonState();
                        revalidate();
                        repaint();
                    }
                });
        mModifyButton.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 4));
        mModifyButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png"));
        sanitizeTinyButton(mModifyButton);
        add(Box.createHorizontalStrut(3));
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void refreshButtons() {
        // TODO: Refresh other buttons in this method too
        if (mRunTime.isRunning(mEditorProject)) {
            if (mRunTime.isPaused(mEditorProject)) {
                mPlayButton.setIcon(ICON_PLAY_STANDARD);
                mPlayButton.setRolloverIcon(ICON_PLAY_ROLLOVER);
                mPlayButton.setToolTipText("Proceed the execution of the sceneflow");
            } else {
                mPlayButton.setIcon(ICON_PAUSE_STANDARD);
                mPlayButton.setRolloverIcon(ICON_PAUSE_ROLLOVER);
                mPlayButton.setToolTipText("Pause the execution of the sceneflow");
            }
        } else {
            mPlayButton.setIcon(ICON_PLAY_STANDARD);
            mPlayButton.setRolloverIcon(ICON_PLAY_ROLLOVER);
            mPlayButton.setToolTipText("Start the execution of the sceneflow");
        }

        // Refresh the element display buttons
        if (mSceneFlowEditor.isElementDisplayVisible()) {
            mElementButton.setIcon(ICON_MORE_STANDARD);
        } else {
            mElementButton.setIcon(ICON_LESS_STANDARD);
        }
        mElementButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelements"))
                ? ICON_MORE_ROLLOVER : ICON_LESS_ROLLOVER);
    }

  
    private void changeModifyButtonState() {
        if (mSceneFlowEditor.isElementEditorVisible()) {
            mModifyButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png"));
        } else {
            mModifyButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png"));
        }
        mModifyButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png"));
    }

    private void changeShowVariablesButtonState() {
        mShowVarButton.setIcon(mSceneFlowEditor.getWorkSpace().isVarBadgeVisible()
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var.png"));
        mShowVarButton.setRolloverIcon(mSceneFlowEditor.getWorkSpace().isVarBadgeVisible()
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden_blue.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_blue.png"));
        mShowVarButton.setToolTipText(mSceneFlowEditor.getWorkSpace().isVarBadgeVisible()
                ? "Hide Variables"
                : "Show Variables");
    }

    private void initPathDisplay() {
        mPathDisplay = new JPanel();    // new FlowLayout(FlowLayout.LEFT, 0, 0));
        mPathDisplay.setLayout(new BoxLayout(mPathDisplay, BoxLayout.X_AXIS));
        mPathDisplay.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));

        // mPathDisplay.setBorder(BorderFactory.createEmptyBorder());
        mPathScrollPane = new JScrollPane(mPathDisplay, JScrollPane.VERTICAL_SCROLLBAR_NEVER,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        mPathScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mPathScrollPane.setMaximumSize(new Dimension(500, 22));
        mPathScrollPane.setMinimumSize(new Dimension(500, 22));
        mPathScrollPane.setPreferredSize(new Dimension(500, 22));
        mPathScrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
        mPathScrollPane.setHorizontalScrollBar(mPathScrollBar);
    }

    public final void refresh() {
        // Print some information
        mLogger.message("Refreshing '" + this + "'");
        // Refresh all components
        refreshButtons();
        refreshDisplay();
        initPreferences();
        // TODO: what else do we need to refresh?

    }

    // Refresh the path display
    private void refreshDisplay() {
        // Remove all path components
        mPathDisplay.removeAll();
        // Get the list of active nodes
        // TODO: Maybe better to have this on sceneflow editor
        final LinkedList<SuperNode> path = mSceneFlowEditor.getSceneFlowManager().getActiveSuperNodes();
        // For each supernode in the path
        for (final SuperNode supernode : path) {
            final Action action = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    //System.err.println("setting level to node " + getValue(Action.NAME));
                    mSceneFlowEditor.getWorkSpace().selectNewWorkSpaceLevel(supernode);
                }
            };
            action.putValue(Action.NAME, supernode.getName());
            action.putValue(Action.SHORT_DESCRIPTION, supernode.getName());
            // Compute color intensity
            int index = mPathDisplay.getComponentCount();
            int intensity = 255 - 5 * index;
            intensity = (intensity < 0) ? 0 : intensity;
            // Create a label with an arrow
            final JLabel arrow = new JLabel("\u2192");
            // Create a button with the name
            final JButton label = new JButton(action);
            label.setUI(new BasicButtonUI());
            label.setBorder(BorderFactory.createLineBorder(Color.GRAY));
            label.setMinimumSize(new Dimension(80, 18));
            label.setMaximumSize(new Dimension(80, 18));
            label.setPreferredSize(new Dimension(80, 18));
            label.setBackground(new Color(intensity, intensity, intensity));
            label.addMouseMotionListener(new MouseMotionAdapter() {
                // TODO: This does not work smouthly, please work over 
                @Override
                public void mouseDragged(MouseEvent e) {
                    int dir = e.getX();

                    if (dir < 0) {
                        mPathScrollBar.setValue(mPathScrollBar.getValue() + 10);
                    } else {
                        mPathScrollBar.setValue(mPathScrollBar.getValue() - 10);
                    }
                }
            });

            if (index > 0) {
                mPathDisplay.add(arrow);
            }
            mPathDisplay.add(label);
        }
        revalidate();
        repaint();
    }
}
