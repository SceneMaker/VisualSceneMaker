package de.dfki.vsm.editor;

import de.dfki.vsm.editor.script.ScriptEditorPanel;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class ProjectEditor extends JSplitPane implements EventListener, Observer {

    //
    private final ProjectData mProject;
    //
    private final SceneFlowEditor mSceneFlowEditor;
    private final ScriptEditorPanel mSceneDocEditor;
    //
    private final Observable mObservable = new Observable();
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventCaster = EventCaster.getInstance();

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    private class Observable extends java.util.Observable {

        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    @Override
    public void update(java.util.Observable obs, Object obj) {
        //mLogger.message("ProjectEditor.update(" + obj + ")");
        mObservable.update(obj);
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    @Override
    public void update(EventObject evt) {
        // mLogger.message("ProjectEditor.update(" + evt + ")");
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public ProjectEditor(ProjectData project) {
        super(JSplitPane.VERTICAL_SPLIT, true);

        mProject = project;
        mSceneFlowEditor = new SceneFlowEditor(mProject.getSceneFlow());
        mSceneDocEditor = new ScriptEditorPanel(mProject.getSceneScript(), mProject.getSceneFlow());

        mObservable.addObserver(mSceneFlowEditor);
        mObservable.addObserver(mSceneDocEditor);

        mEventCaster.append(this);

        initComponents();
    }

    public SceneFlowEditor getSceneFlowEditor() {
        return mSceneFlowEditor;
    }

    public ScriptEditorPanel getSceneDocEditor() {
        return mSceneDocEditor;
    }

    public ProjectData getProject() {
        return mProject;
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void close() {
        if (mProject.hasChanged()) {
            int response = JOptionPane.showConfirmDialog(
                    this, "The project \"" + "?" + "\" has changed.  Save it?",
                    "Save before quitting?",
                    JOptionPane.YES_NO_OPTION);
            if (response == JOptionPane.YES_OPTION) {
                save();
            } else if (response == JOptionPane.CANCEL_OPTION) {
            } else if (response == JOptionPane.NO_OPTION) {
            } else {
            }
        }
        // Delete all observers
        mObservable.deleteObservers();
        // Close / Cleanup
        mSceneFlowEditor.close();
        mSceneDocEditor.close();
    }

    ////////////////////////////////////////////////////////////////////////////
    public void save() {
        if (!mProject.save()) {
            // TODO: Failure Handling
            JOptionPane.showMessageDialog(this,
                    "Wrong Scene Script Syntax.",
                    "Cannot Format Scene Script.",
                    JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    private void initComponents() {
        //
        setBorder(BorderFactory.createEmptyBorder());
        setResizeWeight(Float.valueOf(Preferences.getProperty("sceneflow_sceneeditor_ratio")));
        setOneTouchExpandable(true);

        final Polygon pUp = new Polygon();
        pUp.addPoint(1, 4);
        pUp.addPoint(5, 0);
        pUp.addPoint(9, 4);

        final Polygon pDown = new Polygon();
        pDown.addPoint(13, 0);
        pDown.addPoint(17, 4);
        pDown.addPoint(21, 0);

        setUI(new BasicSplitPaneUI() {

            @Override
            public BasicSplitPaneDivider createDefaultDivider() {
                return new BasicSplitPaneDivider(this) {

                    @Override
                    public void setBorder(Border b) {
                    }

                    @Override
                    public void paint(Graphics g) {
                        Graphics2D graphics = (Graphics2D) g;
                        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                        Rectangle r = getBounds();
                        graphics.setColor(UIManager.getColor("Panel.background"));
                        graphics.fillRect(0, 0, r.width - 1, r.height);
                        graphics.setColor(new Color(100, 100, 100));
                        graphics.fillRect((r.width / 2 - 25), 0, 50, r.height);
                        graphics.drawPolygon(pUp);
                        graphics.fillPolygon(pUp);
                        graphics.drawPolygon(pDown);
                        graphics.fillPolygon(pDown);
                    }
                };
            }
        });

        setDividerSize(5);

        setContinuousLayout(true);

        setTopComponent(mSceneFlowEditor);
        setBottomComponent(mSceneDocEditor);

        // setting size
        boolean showSceneFlowEditor = Boolean.valueOf(Preferences.getProperty("showscenefloweditor"));
        boolean showSceneDocEditor = Boolean.valueOf(Preferences.getProperty("showsceneeditor"));

        if (!showSceneFlowEditor) {
            setDividerLocation(0);
        }

        if (!showSceneDocEditor && showSceneFlowEditor) {
            setDividerLocation(Editor.getInstance().getHeight());
        }

        mSceneDocEditor.addComponentListener(new ComponentListener() {

            @Override
            public void componentResized(ComponentEvent e) {
                if (mSceneFlowEditor.getSize().height == 0) {
                    Preferences.setProperty("showscenefloweditor", "false");
                    Preferences.setProperty("showsceneeditor", "true");
                } else {
                    Preferences.setProperty("showscenefloweditor", "true");
                }
                if (mSceneDocEditor.getSize().height == 0) {
                    Preferences.setProperty("showscenefloweditor", "true");
                    Preferences.setProperty("showsceneeditor", "false");
                } else {
                    Preferences.setProperty("showsceneeditor", "true");
                }
                Preferences.save();
            }

            @Override
            public void componentMoved(ComponentEvent e) {
            }

            @Override
            public void componentShown(ComponentEvent e) {
            }

            @Override
            public void componentHidden(ComponentEvent e) {
            }
        });
    }
}
