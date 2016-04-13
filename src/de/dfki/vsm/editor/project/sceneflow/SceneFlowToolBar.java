package de.dfki.vsm.editor.project.sceneflow;

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.sceneflow.SuperNode;
//import de.dfki.vsm.runtime.RunTimeInstance;
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
import static de.dfki.vsm.Preferences.SCREEN_HORIZONTAL;
import de.dfki.vsm.editor.dialog.OptionsDialog;
import de.dfki.vsm.editor.dialog.SaveFileDialog;
import de.dfki.vsm.editor.event.ElementEditorToggledEvent;
import de.dfki.vsm.editor.event.ProjectChangedEvent;

/**
 * @author Gregor Mehlmann
 */
public class SceneFlowToolBar extends JToolBar implements EventListener {

    /**
     * ************************************************************************************************************************
     * ICONS INITIALIZATION
     * *************************************************************************************************************************
     */
    private final ImageIcon ICON_PLAY_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play.png");
    private final ImageIcon ICON_PLAY_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play_blue.png");
    private final ImageIcon ICON_PLAY_DISABLED = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play_disabled.png");

    private final ImageIcon ICON_STOP_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop.png");
    private final ImageIcon ICON_STOP_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop_blue.png");
    private final ImageIcon ICON_STOP_DISABLED = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop_disabled.png");

    private final ImageIcon ICON_PAUSE_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/pause.png");
    private final ImageIcon ICON_PAUSE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/pause_blue.png");

    private final ImageIcon ICON_MORE_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png");
    private final ImageIcon ICON_MORE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png");

    private final ImageIcon ICON_LESS_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png");
    private final ImageIcon ICON_LESS_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png");

    private final ImageIcon ICON_SAVE_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/save_icon.png");
    private final ImageIcon ICON_SAVE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/save_icon_blue.png");
    private final ImageIcon ICON_SAVE_DISABLED = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/save_icon_disable.png");

    private final ImageIcon ICON_UNDO_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/undo_icon.png");
    private final ImageIcon ICON_UNDO_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/undo_icon_blue.png");
    private final ImageIcon ICON_UNDO_DISABLED = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/undo_icon_disabled.png");

    private final ImageIcon ICON_REDO_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/redo_icon.png");
    private final ImageIcon ICON_REDO_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/redo_icon_blue.png");
    private final ImageIcon ICON_REDO_DISABLED = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/redo_icon_disabled.png");

    private final ImageIcon ICON_NORMALIZE_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/normalize_edges_gray.png");
    private final ImageIcon ICON_NORMALIZE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/normalize_edges_blue.png");

    private final ImageIcon ICON_STRAIGHTEN_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/straighten_gray.png");
    private final ImageIcon ICON_STRAIGHTEN_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/straighten_blue.png");

    private final ImageIcon ICON_VARS_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var.png");
    private final ImageIcon ICON_VARS_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_blue.png");
    private final ImageIcon ICON_VARS_HIDDEN_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden.png");
    private final ImageIcon ICON_VARS_HIDDEN_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden_blue.png");

    private final ImageIcon ICON_STACK_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stack_icon.png");
    private final ImageIcon ICON_STACK_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stack_icon_blue.png");

    private final ImageIcon ICON_UP_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up.png");
    private final ImageIcon ICON_UP_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up_blue.png");

    private final ImageIcon ICON_SCREENSHOT_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/screenshot.png");
    private final ImageIcon ICON_SCREENSHOT_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/screenshot_blue.png");

    private final ImageIcon ICON_ZOOMIN_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomin.png");
    private final ImageIcon ICON_ZOOMIN_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomin_blue.png");

    private final ImageIcon ICON_ZOOMOUT_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomout.png");
    private final ImageIcon ICON_ZOOMOUT_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomout_blue.png");

    private final ImageIcon ICON_SETTINGS_STANDARD = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/settings.png");
    private final ImageIcon ICON_SETTINGS_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/toolbar_icons/settings_blue.png");
    /**
     * ***********************************************************************************************************************
     */

    // The singelton logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The singelton runtime instance
    //private final RunTimeInstance mRunTime = RunTimeInstance.getInstance();
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
    private JButton mTogglePallete;
    private JButton mToggleElementEditor;
    private JButton mPlayButton;
    private JButton mStopButton;
    private JButton mShowVarButton;
    private JButton mStraighten;
    private JButton mNormalize;
    private JButton mSaveProject;
    private JButton mPreferences;
    private JButton mUndo;
    private JButton mRedo;

    //Dimension for buttons
    private Dimension tinyButtonDim = new Dimension(40, 40);
    private Dimension smallButtonDim = new Dimension(50, 40);

    // Path Display GUI Components
    private JPanel mPathDisplay;
    private JScrollBar mPathScrollBar;
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
        //Set maximum size
        setMinimumSize(new Dimension((int) (SCREEN_HORIZONTAL * 0.6), 40));
        //setPreferredSize(new Dimension(SCREEN_HORIZONTAL, 40));
        setMaximumSize(new Dimension(SCREEN_HORIZONTAL, 40));
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
    public void update(EventObject event) {
        if (event instanceof ElementEditorToggledEvent) {
            updateElementEditorButton();
        }
//        if (event instanceof ExceptionThrownEvent) {
//            if (mRunTime.isRunning(mEditorProject)) {
//                mEditorInstance.stop(mEditorProject);
//            }
//        }
        refreshButtons();
        if(event instanceof ProjectChangedEvent )
        {
            if (event.getSource() instanceof OptionsDialog)
            {
                mSaveProject.setEnabled(true);
            }
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

    private void sanitizeButton(JButton b, Dimension bDim) {

        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);
        b.setFocusable(false);
        b.setContentAreaFilled(false);
        b.setText(null);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

    private JSeparator createSeparator() {
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
        mTogglePallete = add(new AbstractAction("ACTION_SHOW_ELEMENTS",
                Boolean.valueOf(Preferences.getProperty("showelements"))
                        ? ICON_MORE_STANDARD
                        : ICON_LESS_STANDARD) {
                    public void actionPerformed(ActionEvent evt) {
                        mSceneFlowEditor.showElementDisplay();
                        refreshButtons();
                    }
                });
        mTogglePallete.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelements"))
                ? ICON_MORE_ROLLOVER
                : ICON_LESS_ROLLOVER);
        sanitizeButton(mTogglePallete, tinyButtonDim);
        add(Box.createHorizontalGlue());
        //Preferences
        mPreferences = add(new AbstractAction("ACTION_SHOW_OPTIONS", ICON_SETTINGS_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.showOptions();
            }
        });
        mPreferences.setRolloverIcon(ICON_SETTINGS_ROLLOVER);
        mPreferences.setToolTipText("Project Preferences");
        sanitizeButton(mPreferences, tinyButtonDim);

        //******************************************************************************************************
        //EDIT PROJECT SECTION
        //Save project
        mSaveProject = add(new AbstractAction("ACTION_SAVEPROJECT", ICON_SAVE_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.save();
                mSaveProject.setEnabled(false);
            }
        });
        mSaveProject.setRolloverIcon(ICON_SAVE_ROLLOVER);
        mSaveProject.setDisabledIcon(ICON_SAVE_DISABLED);
        mSaveProject.setToolTipText("Save current project");
        sanitizeButton(mSaveProject, tinyButtonDim);
        mSaveProject.setEnabled(false);

        //Undo last action
        mUndo = add(new AbstractAction("ACTION_UNDO", ICON_UNDO_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                undoAction.actionPerformed(e);
                refreshButtons();
            }
        });
        mUndo.setRolloverIcon(ICON_UNDO_ROLLOVER);
        mUndo.setDisabledIcon(ICON_UNDO_DISABLED);
        mUndo.setToolTipText("Undo last action");
        sanitizeButton(mUndo, tinyButtonDim);
        mUndo.setEnabled(false);

        //Redo last action
        mRedo = add(new AbstractAction("ACTION_REDO", ICON_REDO_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                redoAction.actionPerformed(e);
                refreshButtons();
            }
        });
        mRedo.setRolloverIcon(ICON_REDO_ROLLOVER);
        mRedo.setDisabledIcon(ICON_REDO_DISABLED);
        mRedo.setToolTipText("Redo last action");
        sanitizeButton(mRedo, tinyButtonDim);
        mRedo.setEnabled(false);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());
        //******************************************************************************************************
        //PROJECT EDITION SECTION 
        // Button to straighten all edeges
        mNormalize = add(new AbstractAction("ACTION_NORMALIZE", ICON_NORMALIZE_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().normalizeAllEdges();
            }
        });
        mNormalize.setRolloverIcon(ICON_NORMALIZE_ROLLOVER);
        mNormalize.setToolTipText("Normalize all edges");
        sanitizeButton(mNormalize, tinyButtonDim);
        // Button to straighten all edeges
        mStraighten = add(new AbstractAction("ACTION_STRAIGHTEN", ICON_STRAIGHTEN_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().straightenAllEdges();
            }
        });
        mStraighten.setRolloverIcon(ICON_STRAIGHTEN_ROLLOVER);
        mStraighten.setToolTipText("Straighten all edges");
        sanitizeButton(mStraighten, tinyButtonDim);
        // The Show Variables Button
        mShowVarButton = add(new AbstractAction("ACTION_SHOW_VARIABLES",
                Boolean.valueOf(Preferences.getProperty("showVariables")) ? ICON_VARS_STANDARD : ICON_VARS_HIDDEN_STANDARD) {
                    public void actionPerformed(ActionEvent evt) {
                        mSceneFlowEditor.getWorkSpace().showVariablesOnWorkspace();
                        updateShowVarsButtons();
                        revalidate();
                        repaint();
                    }
                });
        mShowVarButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showVariables")) ? ICON_VARS_ROLLOVER : ICON_VARS_HIDDEN_ROLLOVER);
        mShowVarButton.setToolTipText(Boolean.valueOf(Preferences.getProperty("showVariables")) ? "Show Variables" : "Hide Variables");
        // Format The Button As Tiny
        sanitizeButton(mShowVarButton, tinyButtonDim);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());
        add(Box.createHorizontalStrut(10));
        // The Play SceneFlow Button
        mPlayButton = add(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.save();
                mEditorInstance.play(mEditorProject);
                mStopButton.setEnabled(true);
            }
        });
        mPlayButton.setIcon(ICON_PLAY_STANDARD);
        mPlayButton.setRolloverIcon(ICON_PLAY_ROLLOVER);
        mPlayButton.setDisabledIcon(ICON_PLAY_DISABLED);
        mPlayButton.setToolTipText("Initialize project/Start the execution of the sceneflow");
        sanitizeButton(mPlayButton, tinyButtonDim);
        // The Stop SceneFlow Button
        mStopButton = add(new AbstractAction("ACTION_STOP", ICON_STOP_STANDARD) {
            @Override
            public final void actionPerformed(ActionEvent e) {
                mEditorInstance.stop(mEditorProject);
                mStopButton.setEnabled(false);
            }
        });
        mStopButton.setRolloverIcon(ICON_STOP_ROLLOVER);
        mStopButton.setDisabledIcon(ICON_STOP_DISABLED);
        mStopButton.setToolTipText("Shutdown project/stop the execution of the sceneflow");
        // Format The Button As Tiny
        sanitizeButton(mStopButton, tinyButtonDim);
        mStopButton.setEnabled(false);

        JButton b = add(new AbstractAction("ACTION_WINDOW", ICON_STACK_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                EditorInstance.getInstance().showMonitor();
            }
        });
        b.setRolloverIcon(ICON_STACK_ROLLOVER);
        b.setToolTipText("Variable Manager");
        sanitizeButton(b, tinyButtonDim);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());

        //******************************************************************************************************
        // CONTROL OF NODES
        // Add Some Horizontal Space
        initPathDisplay();
        add(mPathScrollPane);

        //UP TO PARENT NODE 
        b = add(new AbstractAction("ACTION_LEVEL_UP", ICON_UP_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mSceneFlowEditor.getWorkSpace().decreaseWorkSpaceLevel();
            }
        });
        b.setToolTipText("Up to parent node");
        b.setRolloverIcon(ICON_UP_ROLLOVER);
        sanitizeButton(b, tinyButtonDim);
        add(Box.createHorizontalStrut(10));
        add(createSeparator());
        //******************************************************************************************************
        // SCREEN CONTROL
        //SCREENSHOT BUTTON
        Action action = new AbstractAction("ACTION_SCREEN_SHOT", ICON_SCREENSHOT_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                TransferHandler handler = mSceneFlowEditor.getWorkSpace().getTransferHandler();

                if (handler != null) {
                    handler.exportToClipboard(mSceneFlowEditor.getWorkSpace(), mSystemClipBoard, TransferHandler.COPY);
                    SaveFileDialog fileChooser = new SaveFileDialog();
                    fileChooser.save();
                } else {
                    System.err.println("handler null");
                }
            }
        };
        action.putValue(Action.SHORT_DESCRIPTION, "Add/Remove window");
        b = add(action);
        b.setToolTipText("Take a screenshot");
        b.setRolloverIcon(ICON_SCREENSHOT_ROLLOVER);

        //ZOOM IN BUTTON
        sanitizeButton(b, smallButtonDim);
        b = add(new AbstractAction("ACTION_ZOOM_IN", ICON_ZOOMIN_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize < 190)
                        ? mNodeSize = mNodeSize + 10
                        : mNodeSize;
                saveEditorConfig();
            }
        });
        b.setToolTipText("Zoom In");
        b.setRolloverIcon(ICON_ZOOMIN_ROLLOVER);

        //ZOOM OUT BUTTON
        sanitizeButton(b, smallButtonDim);
        b = add(new AbstractAction("ACTION_ZOOM_OUT", ICON_ZOOMOUT_STANDARD) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize > 30)
                        ? mNodeSize = mNodeSize - 10
                        : mNodeSize;
                saveEditorConfig();
            }
        });
        b.setToolTipText("Zoom Out");
        b.setRolloverIcon(ICON_ZOOMOUT_ROLLOVER);
        sanitizeButton(b, smallButtonDim);

        //
        // Property Space
        //
        add(Box.createHorizontalGlue());
        mToggleElementEditor = add(new AbstractAction("ACTION_SHOW_ELEMENTPROP",
                Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                        ? ICON_LESS_STANDARD
                        : ICON_MORE_STANDARD) {
                    public void actionPerformed(ActionEvent evt) {
                        mSceneFlowEditor.toggleElementEditor();
                        updateElementEditorButton();

                    }
                });
        mToggleElementEditor.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 4));
        mToggleElementEditor.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? ICON_LESS_ROLLOVER
                : ICON_MORE_ROLLOVER);
        sanitizeButton(mToggleElementEditor, tinyButtonDim);
        add(Box.createHorizontalStrut(3));
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void refreshButtons() {
        // Print some information
        //mLogger.message("Refreshing Buttons Of '" + this + "'");
        //*************************************
        //Refresh the buttons SAVE, UNDO and REDO when project have been changed
        mSaveProject.setEnabled(mEditorProject.hasChanged());
        mUndo.setEnabled(undoAction.isEnabled());
        mRedo.setEnabled(redoAction.isEnabled());
        //*************************************
        //mLogger.message("Check execution status '" + this + "'");
        //refresh the play button when running the scene player
        if (/*mRunTime.isRunning(mEditorProject)*/mEditorProject.isRunning()) {
            // Print some information
            //mLogger.message("Running");
            if (/*mRunTime.isPaused(mEditorProject)*/mEditorProject.isPaused()) {
                // Print some information
                //mLogger.message("Paused");
                mPlayButton.setIcon(ICON_PLAY_STANDARD);
                mPlayButton.setRolloverIcon(ICON_PLAY_ROLLOVER);
                mPlayButton.setToolTipText("Proceed the execution of the sceneflow");
            } else {
                // Print some information
                //mLogger.message("Playing");
                mPlayButton.setIcon(ICON_PAUSE_STANDARD);
                mPlayButton.setRolloverIcon(ICON_PAUSE_ROLLOVER);
                mPlayButton.setToolTipText("Pause the execution of the sceneflow");
            }
        } else {
            // Print some information
            //mLogger.message("Not Running");
            mPlayButton.setIcon(ICON_PLAY_STANDARD);
            mPlayButton.setRolloverIcon(ICON_PLAY_ROLLOVER);
            
            mPlayButton.setToolTipText("Initialize project/Start the execution of the sceneflow");
            // if an execution has been ended disable the play button
            if (/*mRunTime.wasExecuted(mEditorProject)*/mEditorProject.wasExecuted() && mStopButton.isEnabled()) {
                mPlayButton.setEnabled(false);
            } else if (!/*mRunTime.isRunning(mEditorProject)*/mEditorProject.isRunning()){
                mPlayButton.setEnabled(true);
                mStopButton.setEnabled(false);
            }

        }
        //*************************************
        // Refresh the element display buttons
        mTogglePallete.setIcon(mSceneFlowEditor.isElementDisplayVisible()
                ? ICON_MORE_STANDARD : ICON_LESS_STANDARD);
        mTogglePallete.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelements"))
                ? ICON_MORE_ROLLOVER : ICON_LESS_ROLLOVER);
    }

    private void updateElementEditorButton() {
        mToggleElementEditor.setIcon(mSceneFlowEditor.isElementEditorVisible()
                ? ICON_LESS_STANDARD : ICON_MORE_STANDARD);
        mToggleElementEditor.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? ICON_LESS_ROLLOVER : ICON_MORE_ROLLOVER);
    }

    private void updateShowVarsButtons() {
        mShowVarButton.setIcon(mSceneFlowEditor.getWorkSpace().isVarBadgeVisible()
                ? ICON_VARS_HIDDEN_STANDARD
                : ICON_VARS_STANDARD);
        mShowVarButton.setRolloverIcon(mSceneFlowEditor.getWorkSpace().isVarBadgeVisible()
                ? ICON_VARS_HIDDEN_ROLLOVER
                : ICON_VARS_ROLLOVER);
        mShowVarButton.setToolTipText(mSceneFlowEditor.getWorkSpace().isVarBadgeVisible()
                ? "Hide Variables"
                : "Show Variables");
    }

    private void initPathDisplay() {
        mPathDisplay = new JPanel();    // new FlowLayout(FlowLayout.LEFT, 0, 0));
        mPathDisplay.setLayout(new BoxLayout(mPathDisplay, BoxLayout.X_AXIS));
        //mPathDisplay.setMaximumSize(new Dimension(500, 22));
        mPathDisplay.setMinimumSize(new Dimension(500, 22));
        //mPathDisplay.setPreferredSize(new Dimension(500, 22));

        // mPathDisplay.setBorder(BorderFactory.createEmptyBorder());
        mPathScrollPane = new JScrollPane(mPathDisplay);
        mPathScrollPane.setViewportBorder(BorderFactory.createLineBorder(Color.gray));
        mPathScrollPane.setMaximumSize(new Dimension(500, 40));
        mPathScrollPane.setMinimumSize(new Dimension(300, 30));
        mPathScrollPane.setPreferredSize(new Dimension(300, 30));
        mPathScrollPane.setBorder(BorderFactory.createEmptyBorder());
        mPathScrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
        mPathScrollBar.setPreferredSize(new Dimension(300, 10));
        mPathScrollBar.setUI(new WindowsScrollBarUI());
        mPathScrollBar.setOpaque(false);
        mPathScrollBar.setBorder(BorderFactory.createEmptyBorder());
        mPathScrollPane.setHorizontalScrollBar(mPathScrollBar);
    }

    public final void refresh() {
        // Print some information
        //mLogger.message("Refreshing '" + this + "'");
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
            final JButton pathElement = new JButton(action);
            pathElement.setUI(new BasicButtonUI());
            pathElement.setBorder(BorderFactory.createLineBorder(Color.GRAY));
            pathElement.setMinimumSize(new Dimension(80, 18));
            pathElement.setMaximumSize(new Dimension(80, 18));
            pathElement.setPreferredSize(new Dimension(80, 18));
            pathElement.setBackground(new Color(intensity, intensity, intensity));
            pathElement.addMouseMotionListener(new MouseMotionAdapter() {
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
            mPathDisplay.add(pathElement);
        }
        revalidate();
        repaint();
    }
}
