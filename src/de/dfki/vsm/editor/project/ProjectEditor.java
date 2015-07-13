package de.dfki.vsm.editor.project;

import de.dfki.vsm.editor.SceneFlowEditor;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.editor.script.SceneScriptEditor;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.JSplitPane;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class ProjectEditor extends JSplitPane implements EventListener, Observer {

    // The singelton logger instance   
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The singelton event multicaster
    private final EventDispatcher mEventDispatcher = EventDispatcher.getInstance();
    // The editor project of this editor
    private final EditorProject mEditorProject;
    // The sceneflow editor of this project
    private final SceneFlowEditor mSceneFlowEditor;
    // The scenescript editor of this project
    private final SceneScriptEditor mSceneScriptEditor;
    // The editor's observable component 
    private final Observable mObservable = new Observable();

    // Create an empty project editor
    public ProjectEditor() {
        this(new EditorProject());
    }

    // Construct a project editor with a project
    public ProjectEditor(final EditorProject project) {
        // Initialize the parent split pane
        super(JSplitPane.VERTICAL_SPLIT, true);
        // Initialize the editor project
        mEditorProject = project;
        // Initialize the sceneflow editor
        mSceneFlowEditor = new SceneFlowEditor(mEditorProject);
        mObservable.addObserver(mSceneFlowEditor);
        // Initialize the scenescript editor
        mSceneScriptEditor = new SceneScriptEditor(mEditorProject);
        mObservable.addObserver(mSceneScriptEditor);
        // Register at the event dispatcher
        mEventDispatcher.register(this);
        // Initialize the GUI components
        initComponents();
    }

    // Get the sceneflow editor 
    public final SceneFlowEditor getSceneFlowEditor() {
        return mSceneFlowEditor;
    }

    // Get the scenescript editor 
    public final SceneScriptEditor getSceneScriptEditor() {
        return mSceneScriptEditor;
    }

    // Get the editor project 
    public final EditorProject getEditorProject() {
        return mEditorProject;
    }

    // Clean up the editor component
    public final void close() {
        // Delete all observer objects
        mObservable.deleteObservers();
        // Remove from event dispatcher
        mEventDispatcher.remove(this);
        // Close / Cleanup Members
        mSceneFlowEditor.close();
        mSceneScriptEditor.close();
    }

    // Initialize the GUI components
    private void initComponents() {
        // Set Background Color
        setBackground(Color.WHITE);
        // Set An Empty Border
        setBorder(BorderFactory.createEmptyBorder());

        setOneTouchExpandable(true);

        setResizeWeight(Float.valueOf(Preferences.getProperty("sceneflow_sceneeditor_ratio")));

        setUI(new BasicSplitPaneUI() {

            @Override
            public BasicSplitPaneDivider createDefaultDivider() {
                return new BasicSplitPaneDivider(this) {
                    /**
                     * Shows the bottom part of the editor when mouse goes over
                     * the border
                     *
                     * @param me
                     */
                    @Override
                    protected void processMouseEvent(MouseEvent me) {
                        super.processMouseEvent(me);
                        switch (me.getID()) {

                            case MouseEvent.MOUSE_ENTERED:
                                if (!mSceneScriptEditor.isPinPricked()) {
                                    showSceneScriptEditor();
                                }
                                break;
                            case MouseEvent.MOUSE_RELEASED:
                                Preferences.setProperty("propertiesdividerlocation", String.valueOf(((ProjectEditor) this.getParent()).getDividerLocation()));
                                mSceneScriptEditor.prickPin();
                                break;
                        }
                    }

                };

            }

        });

        setDividerSize(10);

        setContinuousLayout(true);
        mSceneFlowEditor.setMinimumSize(new Dimension(10, 10));
        mSceneFlowEditor.setMaximumSize(new Dimension(10000, 3000));
        setTopComponent(mSceneFlowEditor);
        mSceneScriptEditor.setMinimumSize(new Dimension(10, 10));
        mSceneScriptEditor.setMaximumSize(new Dimension(10000, 3000));
        setBottomComponent(mSceneScriptEditor);

        // setting size
        boolean showSceneFlowEditor = Boolean.valueOf(Preferences.getProperty("showscenefloweditor"));
        boolean showSceneDocEditor = Boolean.valueOf(Preferences.getProperty("showsceneeditor"));

        if (!showSceneFlowEditor) {
            setDividerLocation(1d);
        }

        if (showSceneDocEditor && showSceneFlowEditor) {
            setDividerLocation(Integer.parseInt(Preferences.getProperty("propertiesdividerlocation")));
        }

        mSceneScriptEditor.addComponentListener(
                new ComponentListener() {

                    @Override
                    public void componentResized(ComponentEvent e
                    ) {
                        if (mSceneFlowEditor.getSize().height == 0) {
                            Preferences.setProperty("showscenefloweditor", "false");
                            Preferences.setProperty("showsceneeditor", "true");
                        } else {
                            Preferences.setProperty("showscenefloweditor", "true");
                        }
                        if (mSceneScriptEditor.getSize().height == 0) {
                            Preferences.setProperty("showscenefloweditor", "true");
                            Preferences.setProperty("showsceneeditor", "false");
                        } else {
                            Preferences.setProperty("showsceneeditor", "true");
                        }
                        Preferences.save();
                    }

                    @Override
                    public void componentMoved(ComponentEvent e
                    ) {
                    }

                    @Override
                    public void componentShown(ComponentEvent e
                    ) {
                    }

                    @Override
                    public void componentHidden(ComponentEvent e
                    ) {
                    }
                }
        );
    }

    // Show the bottom part of the project editor
    private final void showSceneScriptEditor() {
        setDividerLocation(
                // TODO: Do we really need to parse this every time here?
                Integer.parseInt(Preferences.getProperty("propertiesdividerlocation")));
    }

    // Hides the bottom part of the project editor
    private final void hideSceneScriptEditor() {
        setDividerLocation(1d);
    }

    // The observable class of the editor
    private final class Observable extends java.util.Observable {

        public final void update(final Object object) {
            setChanged();
            notifyObservers(object);
        }
    }

    // Recursively notify observers
    @Override
    public void update(java.util.Observable obs, Object obj) {
        mObservable.update(obj);
    }

    // Update when an event happened  
    @Override
    public void update(final EventObject event) {
        if (event instanceof FunctionSelectedEvent
                || event instanceof TreeEntrySelectedEvent) {
            {
                // Show the scenescript editor
                showSceneScriptEditor();
            }
        }
        // TODO: I do not like this conceptually
        if (event instanceof NodeSelectedEvent
                && !mSceneScriptEditor.isPinPricked()) {
            // Hide the scenescript editor
            hideSceneScriptEditor();
        }
    }

}
