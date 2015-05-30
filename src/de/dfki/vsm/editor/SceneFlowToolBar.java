package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.event.SceneStoppedEvent;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.runtime.RunTime;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

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
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.TransferHandler;
import javax.swing.plaf.basic.BasicButtonUI;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebahrd
 */
public class SceneFlowToolBar extends JToolBar implements Observer {

    // The VSM Runtime Instance
    private final RunTime mRunTime = RunTime.getInstance();

    // The Parent Editor Window
    private final Editor mWindow = Editor.getInstance();

    // Clipboard
    final Clipboard                     clipboard       = getToolkit().getSystemClipboard();
    private final LinkedList<SuperNode> mPathComponents = new LinkedList<>();
    private final LOGDefaultLogger      mLogger         = LOGDefaultLogger.getInstance();
    private final EventCaster           mEventCaster    = EventCaster.getInstance();
    private final Editor                mSMEditor       = Editor.getInstance();

    // The Parent SceneFlow Editor
    private final SceneFlowEditor mEditor;
    private final SceneFlow mSceneFlow;

    //
    private int mNodeSize;    // only one dimension

    // The Button Components
    private JButton mElementButton;
    private JButton mModifyButton;
    private JButton mPlayButton;
    private JButton mStopButton;
    private JButton mShowVarButton;
    private JButton mStraighten;
    private JButton mNormalize;

    // Path Display GUI Components
    private JPanel      mPathDisplay;
    private JScrollBar  mPathScrollBar;
    private JScrollPane mPathScrollPane;

    //
    private final ProjectData mProject;
    private final ProjectPreferences mPreferences;

    public SceneFlowToolBar(final SceneFlowEditor sceneFlowEditor, ProjectData project) {
        super("Navigation Bar", JToolBar.HORIZONTAL);

        // Initialize The Editor
        mEditor      = sceneFlowEditor;
        mProject     = sceneFlowEditor.getWorkSpace().getProject();
        mPreferences = sceneFlowEditor.getWorkSpace().getPreferences();

        // Initialize The SceneFlow
        mSceneFlow = mEditor.getSceneFlow();
        setBackground(Color.white);
        setFloatable(false);
        setRollover(true);
        setBorder(BorderFactory.createEmptyBorder(4, 0, 4, 0));
        initPreferences();
        initComponents();
    }

    @Override
    public void update(Observable obs, Object obj) {

        // mLogger.message("SceneFlowToolBar.update");
        updatePathDisplay();
        updatePathText();
        initPreferences();
    }

    public void updatePathDisplay() {
        mPathComponents.clear();

        for (SuperNode superNode : mEditor.getSceneFlowManager().getActiveSuperNodes()) {
            mPathComponents.add(superNode);
        }

        updatePathText();
    }

    private void initPreferences() {
        if (mSMEditor.getSelectedProjectEditor() != null) {
            for (Object keyObj : mPreferences.getKeySet()) {
                String key = (String) keyObj;

                if (key.equals("node_width")) {
                    mNodeSize = Integer.valueOf(mPreferences.getProperty(key));
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

    private void savePreferences() {
        mPreferences.setProperty("node_width", Integer.toString(mNodeSize));
        mPreferences.setProperty("node_height", Integer.toString(mNodeSize));
        mPreferences.save(mProject.getPreferencesFileName());
        Editor.getInstance().update();
    }

    public void addPathComponent(SuperNode supernode) {
        mPathComponents.addLast(supernode);
        updatePathText();

        int va = mPathScrollBar.getMaximum();

        mPathScrollBar.setValue(va);
    }

    public SuperNode removePathComponent() {
        SuperNode sn = mPathComponents.removeLast();

        // String str = mPathComponents.removeLast();

        updatePathText();

        return sn;
    }

    public void setPathComponent(int index, SuperNode supernode) {
        mPathComponents.set(index, supernode);
        updatePathText();
    }

    private void updatePathText() {
        mPathDisplay.removeAll();

        for (final SuperNode sn : mPathComponents) {
            Action action = new AbstractAction("ACTION_SET_LEVEL") {
                public void actionPerformed(ActionEvent e) {
                    //System.err.println("setting level to node " + getValue(Action.NAME));
                    mEditor.getWorkSpace().selectNewWorkSpaceLevel(sn);
                }
            };

            action.putValue(Action.SHORT_DESCRIPTION, sn.getName());
            action.putValue(Action.NAME, sn.getName());
            JLabel lab = new JLabel("\u2192");
            JButton label = new JButton(action);

            label.setUI(new BasicButtonUI());
            label.setBorder(BorderFactory.createLineBorder(Color.GRAY));

            // label.setFont(new Font("Arial", Font.ITALIC, 10));
            label.setMinimumSize(new Dimension(80, 18));
            label.setMaximumSize(new Dimension(80, 18));
            label.setPreferredSize(new Dimension(80, 18));

            int compCnt = mPathDisplay.getComponentCount();
            int gray = 255 - 5 * compCnt;

            gray = (gray < 0)
                    ? 0
                    : gray;
            label.setBackground(new Color(gray, gray, gray));
            label.addMouseMotionListener(new MouseMotionAdapter() {

                // moving the content
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

            if (compCnt > 0) {
                mPathDisplay.add(lab);
            }

            mPathDisplay.add(label);
        }

        revalidate();
        repaint();
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

    /**
     *
     */
    private void initComponents() {

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
                        mEditor.showElementDisplay();
                        changeElementButtonState();
                        revalidate();
                        repaint();
                    }
                });
        mElementButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelements"))
                                       ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                                       : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
        sanitizeTinyButton(mElementButton);
        add(Box.createHorizontalGlue());

        // Button to straighten all edeges
        mNormalize = add(new AbstractAction("ACTION_NORMALIZE", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/normalize_edges_gray.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mWindow.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().normalizeAllEdges();
            }
        });
        mNormalize.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/normalize_edges_blue.png"));
        mNormalize.setToolTipText("Normalize all edges");
        sanitizeTinyButton(mNormalize);
        add(Box.createHorizontalStrut(3));
        // Button to straighten all edeges
        mStraighten = add(new AbstractAction("ACTION_STRAIGHTEN", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/straighten_gray.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mWindow.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().straightenAllEdges();
            }
        });
        mStraighten.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/straighten_blue.png"));
        mStraighten.setToolTipText("Straighten all edges");
        sanitizeTinyButton(mStraighten);

        add(Box.createHorizontalStrut(30));
        addSeparator();
        add(Box.createHorizontalStrut(30));
        // The Play SceneFlow Button
        mPlayButton = add(new AbstractAction("ACTION_PLAY", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                actionStartSceneFlow();
            }
        });
        mPlayButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play_blue.png"));
        mPlayButton.setToolTipText("Play Scene");
        sanitizeTinyButton(mPlayButton);

        add(Box.createHorizontalStrut(3));

        // The Stop SceneFlow Button
        mStopButton = add(new AbstractAction("ACTION_STOP", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop.png")) {
            @Override
            public final void actionPerformed(ActionEvent e) {
                actionStopSceneFlow();
            }
        });
        mStopButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stop_blue.png"));
        mStopButton.setToolTipText("Stop Scene");
        // Format The Button As Tiny
        sanitizeTinyButton(mStopButton);

        // Add Some Horizontal Space
        add(Box.createHorizontalStrut(3));

        JButton b = add(new AbstractAction("ACTION_WINDOW", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/window.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                Editor.getInstance().showMonitor();
            }
        });
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/window_blue.png"));
        b.setToolTipText("Settings");
        sanitizeTinyButton(b);
        addSeparator();

        // The Show Variables Button
        mShowVarButton = add(new AbstractAction("ACTION_SHOW_VARIABLES",
                Boolean.valueOf(Preferences.getProperty("showVariables"))
                        ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var.png")
                        : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden.png")) {
                    public void actionPerformed(ActionEvent evt) {
                        mEditor.getWorkSpace().showVariablesOnWorkspace();
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
        // Add Some Horizontal Space
        add(Box.createHorizontalStrut(3));

        addSeparator();
        initPathDisplay();
        add(mPathScrollPane);

        add(Box.createHorizontalStrut(3));
        b = add(new AbstractAction("ACTION_LEVEL_UP", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditor.getWorkSpace().decreaseWorkSpaceLevel();
            }
        });
        b.setToolTipText("Up to parent node");
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up_blue.png"));

        sanitizeTinyButton(b);
        addSeparator();

        Action action = new AbstractAction("ACTION_SCREEN_SHOT",
                ResourceLoader.loadImageIcon("/res/img/toolbar_icons/screenshot.png")) {
                    @Override
                    public void actionPerformed(ActionEvent evt) {
                        TransferHandler handler = mEditor.getWorkSpace().getTransferHandler();

                if (handler != null) {
                    handler.exportToClipboard(mEditor.getWorkSpace(), clipboard, TransferHandler.COPY);
                } else {
                    System.err.println("handler null");
                }
            }
        };
        action.putValue(Action.SHORT_DESCRIPTION, "Add/Remove window");
        b = add(action);
        b.setToolTipText("Take a screenshot");
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/screenshot_blue.png"));
        sanitizeSmallButton(b);
        add(Box.createHorizontalStrut(3));
        b = add(new AbstractAction("ACTION_ZOOM_IN", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomin.png")) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize < 190)
                        ? mNodeSize = mNodeSize + 10
                        : mNodeSize;
                savePreferences();
            }
        });
        b.setToolTipText("Zoom In");
        b.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomin_blue.png"));
        sanitizeSmallButton(b);
        add(Box.createHorizontalStrut(3));
        b = add(new AbstractAction("ACTION_ZOOM_OUT", ResourceLoader.loadImageIcon("/res/img/toolbar_icons/zoomout.png")) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize > 30)
                        ? mNodeSize = mNodeSize - 10
                        : mNodeSize;
                savePreferences();
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
                        mEditor.showElementEditor();
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
    public final void actionStopSceneFlow() {

        // Stop The Execution
        mWindow.stopSceneFlow();

        // Update The Buttons
        changeRuntimeButtonState();

        // un select nodes and edges
        SceneStoppedEvent ev = new SceneStoppedEvent(this);
        mEventCaster.convey(ev);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void actionStartSceneFlow() {

        // Check State Of Execution
        if (mRunTime.isSceneFlowRunnning(mSceneFlow)) {
            mWindow.pauseSceneFlow();
        } else {
            mWindow.startSceneFlow();
        }

        // Update The Buttons
        changeRuntimeButtonState();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void changeRuntimeButtonState() {

        //
        if (mRunTime.isSceneFlowRunnning(mSceneFlow)) {
            if (mRunTime.isSceneFlowPaused(mSceneFlow)) {
                mPlayButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play.png"));
                mPlayButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play_blue.png"));
                mPlayButton.setToolTipText("Play Scene");
            } else {
                mPlayButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/pause.png"));
                mPlayButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/pause_blue.png"));
                mPlayButton.setToolTipText("Pause Scene");
            }
        } else {
            mPlayButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play.png"));
            mPlayButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/play_blue.png"));
            mPlayButton.setToolTipText("Play Scene");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void changeElementButtonState() {
        if (mEditor.isElementDisplayVisible()) {
            mElementButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png"));
        } else {
            mElementButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png"));
        }
        mElementButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelements"))
                                       ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                                       : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
    }

    private void changeModifyButtonState() {
        if (mEditor.isElementEditorVisible()) {
            mModifyButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png"));
        } else {
            mModifyButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png"));
        }
        mModifyButton.setRolloverIcon(Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                                      ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png")
                                      : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png"));
    }

    private void changeShowVariablesButtonState() {
        mShowVarButton.setIcon(mEditor.getWorkSpace().isVarBadgeVisible()
                               ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden.png")
                               : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var.png"));
        mShowVarButton.setRolloverIcon(mEditor.getWorkSpace().isVarBadgeVisible()
                                       ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_hidden_blue.png")
                                       : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/var_blue.png"));
        mShowVarButton.setToolTipText(mEditor.getWorkSpace().isVarBadgeVisible()
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
}
