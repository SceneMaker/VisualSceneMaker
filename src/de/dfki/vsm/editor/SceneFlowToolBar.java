package de.dfki.vsm.editor;

import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.runtime.RunTime;
import de.dfki.vsm.util.evt.EventCaster;
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
    private final RunTime mRunTime
            = RunTime.getInstance();
    // The Parent Editor Window
    private final Editor mWindow
            = Editor.getInstance();
    // The Parent SceneFlow Editor
    private final SceneFlowEditor mEditor;
    private final SceneFlow mSceneFlow;
    //
    private int mNodeSize; //only one dimension
    // The Button Components
    private JButton mElementButton;
    private JButton mModifyButton;
    private JButton mPlayButton;
    private JButton mStopButton;
    // Path Display GUI Components
    private JPanel mPathDisplay;
    private JScrollBar mPathScrollBar;
    private JScrollPane mPathScrollPane;
    // Clipboard
    final Clipboard clipboard = getToolkit().getSystemClipboard();
    private final LinkedList<String> mPathComponents = new LinkedList<String>();
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventCaster = EventCaster.getInstance();

    public SceneFlowToolBar(final SceneFlowEditor sceneFlowEditor) {
        super("Navigation Bar", JToolBar.HORIZONTAL);
        // Initialize The Editor
        mEditor = sceneFlowEditor;
        // Initialize The SceneFlow
        mSceneFlow = mEditor.getSceneFlow();
        //
        setFloatable(false);
        setRollover(true);
        setBorder(BorderFactory.createEmptyBorder(4, 0, 4, 0));
        initPreferences();
        initComponents();
    }

    @Override
    public void update(Observable obs, Object obj) {
        //mLogger.message("SceneFlowToolBar.update");
        updatePathDisplay();
        updatePathText();

    }

    public void updatePathDisplay() {
        mPathComponents.clear();

        for (SuperNode superNode : mEditor.getSceneFlowManager().getActiveSuperNodes()) {
            mPathComponents.add(superNode.getName());
        }
        updatePathText();
    }

    private void initPreferences() {
        for (Object keyObj : Preferences.getKeySet()) {
            String key = (String) keyObj;

            if (key.equals("node_width")) {
                mNodeSize = Integer.valueOf(Preferences.getProperty(key));
            }
        }
    }

    private void savePreferences() {
        Preferences.setProperty("node_width", Integer.toString(mNodeSize));
        Preferences.setProperty("node_height", Integer.toString(mNodeSize));
        Preferences.save();
        Editor.getInstance().update();
    }

    public void addPathComponent(String value) {
        mPathComponents.addLast(value);
        updatePathText();
        int va = mPathScrollBar.getMaximum();

        mPathScrollBar.setValue(va);
    }

    public String removePathComponent() {
        String str = mPathComponents.removeLast();
        updatePathText();
        return str;
    }

    public void setPathComponent(int index, String value) {
        mPathComponents.set(index, value);
        updatePathText();
    }

    private void updatePathText() {
        mPathDisplay.removeAll();
        for (String str : mPathComponents) {
            Action action = new AbstractAction("ACTION_SET_LEVEL") {
                public void actionPerformed(ActionEvent e) {
                    //System.err.println("setting level to node " + getValue(Action.NAME));
                    mEditor.getWorkSpace().selectNewWorkSpaceLevel((String) getValue(Action.NAME));
                }
            };
            action.putValue(Action.SHORT_DESCRIPTION, str);
            action.putValue(Action.NAME, str);
            JLabel lab = new JLabel("\u2192");

            JButton label = new JButton(action);
            label.setUI(new BasicButtonUI());
            label.setBorder(BorderFactory.createLineBorder(Color.GRAY));
            //label.setFont(new Font("Arial", Font.ITALIC, 10));
            label.setMinimumSize(new Dimension(80, 18));
            label.setMaximumSize(new Dimension(80, 18));
            label.setPreferredSize(new Dimension(80, 18));
            int compCnt = mPathDisplay.getComponentCount();
            int gray = 255 - 5 * compCnt;
            gray = (gray < 0) ? 0 : gray;
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
        Dimension bDim = new Dimension(22, 22);
        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

    private void sanitizeSmallButton(JButton b) {
        Dimension bDim = new Dimension(30, 22);
        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        b.setOpaque(false);
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

        mElementButton = add(new AbstractAction("ACTION_SHOW_ELEMENTS",
                Boolean.valueOf(Preferences.getProperty("showelements"))
                ? ResourceLoader.loadImageIcon("/res/img/new/less.png")
                : ResourceLoader.loadImageIcon("/res/img/new/more.png")) {
                    public void actionPerformed(ActionEvent evt) {
                        mEditor.showElementDisplay();
                        changeElementButtonState();
                        revalidate();
                        repaint();
                    }
                });
        sanitizeTinyButton(mElementButton);

        add(Box.createHorizontalGlue());

        // The Stop SceneFlow Button
        mPlayButton = add(new AbstractAction("ACTION_PLAY", ResourceLoader.loadImageIcon("/res/img/new/play.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                actionStartSceneFlow();
            }
        });
        sanitizeTinyButton(mPlayButton);

        add(Box.createHorizontalStrut(2));

        // The Stop SceneFlow Button
        mStopButton = add(
                new AbstractAction("ACTION_STOP",
                        ResourceLoader.loadImageIcon("/res/img/new/stop.png")) {
                    @Override
                    public final void actionPerformed(ActionEvent e) {
                        actionStopSceneFlow();
                    }
                });
        // Format The Button As Tiny
        sanitizeTinyButton(mStopButton);
        // Add Some Horizontal Space
        add(Box.createHorizontalStrut(2));

        JButton b = add(new AbstractAction("ACTION_WINDOW", ResourceLoader.loadImageIcon("/res/img/new/window.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                Editor.getInstance().showMonitor();
            }
        });
        sanitizeTinyButton(b);

        addSeparator();

        initPathDisplay();
        add(mPathScrollPane);

        add(Box.createHorizontalStrut(2));

        b = add(new AbstractAction("ACTION_LEVEL_UP", ResourceLoader.loadImageIcon("/res/img/new/up.png")) {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditor.getWorkSpace().decreaseWorkSpaceLevel();
            }
        });
        sanitizeTinyButton(b);

        addSeparator();

        Action action = new AbstractAction("ACTION_SCREEN_SHOT", ResourceLoader.loadImageIcon("/res/img/new/screenshot.png")) {
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
        sanitizeSmallButton(b);

        add(Box.createHorizontalStrut(2));

        b = add(new AbstractAction("ACTION_ZOOM_IN", ResourceLoader.loadImageIcon("/res/img/new/zoom_plus.png")) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize < 190) ? mNodeSize=mNodeSize+10 : mNodeSize;
                savePreferences();
            }
        });
        sanitizeTinyButton(b);

        add(Box.createHorizontalStrut(2));

        b = add(new AbstractAction("ACTION_ZOOM_OUT", ResourceLoader.loadImageIcon("/res/img/new/zoom_minus.png")) {
            @Override
            public void actionPerformed(ActionEvent evt) {
                mNodeSize = (mNodeSize > 30) ? mNodeSize=mNodeSize-10: mNodeSize;
                savePreferences();
            }
        });
        sanitizeTinyButton(b);

        //
        // Property Space
        //
        add(Box.createHorizontalGlue());

        mModifyButton = add(new AbstractAction("ACTION_SHOW_ELEMENTPROP",
                Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? ResourceLoader.loadImageIcon("/res/img/new/less.png")
                : ResourceLoader.loadImageIcon("/res/img/new/more.png")) {
                    public void actionPerformed(ActionEvent evt) {
                        mEditor.showElementEditor();
                        changeModifyButtonState();

                        revalidate();
                        repaint();

                    }
                });
        sanitizeTinyButton(mModifyButton);
        add(Box.createHorizontalStrut(2));
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void actionStopSceneFlow() {
        // Stop The Execution
        mWindow.stopSceneFlow();
        // Update The Buttons
        changeRuntimeButtonState();
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
    private void changeRuntimeButtonState() {
        //
        if (mRunTime.isSceneFlowRunnning(mSceneFlow)) {
            if (mRunTime.isSceneFlowPaused(mSceneFlow)) {
                mPlayButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/play_pause.png"));
            } else {
                mPlayButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/pause.png"));
            }
        } else {
            mPlayButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/play.png"));
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void changeElementButtonState() {
        if (mEditor.isElementDisplayVisible()) {
            mElementButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/less.png"));
        } else {
            mElementButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/more.png"));
        }
    }

    private void changeModifyButtonState() {
        if (mEditor.isElementEditorVisible()) {
            mModifyButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/less.png"));
        } else {
            mModifyButton.setIcon(ResourceLoader.loadImageIcon("/res/img/new/more.png"));
        }
    }

    private void initPathDisplay() {
        mPathDisplay = new JPanel();//new FlowLayout(FlowLayout.LEFT, 0, 0));
        mPathDisplay.setLayout(new BoxLayout(mPathDisplay, BoxLayout.X_AXIS));
        mPathDisplay.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));
        //mPathDisplay.setBorder(BorderFactory.createEmptyBorder());
        mPathScrollPane = new JScrollPane(mPathDisplay,
                JScrollPane.VERTICAL_SCROLLBAR_NEVER,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        mPathScrollPane.setBorder(BorderFactory.createEmptyBorder());
        //mPathScrollPane.setBorder(BorderFactory.createEtchedBorder());
        mPathScrollPane.setMaximumSize(new Dimension(500, 22));
        mPathScrollPane.setMinimumSize(new Dimension(500, 22));
        mPathScrollPane.setPreferredSize(new Dimension(500, 22));
        mPathScrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
        mPathScrollPane.setHorizontalScrollBar(mPathScrollBar);
    }
}
