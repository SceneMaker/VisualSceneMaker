package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.editor.event.WorkSpaceSelectedEvent;
import de.dfki.vsm.editor.script.ScriptEditorPanel;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;

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
    private final Observable       mObservable   = new Observable();
    private final LOGDefaultLogger mLogger       = LOGDefaultLogger.getInstance();
    private final EventCaster      mEventCaster  = EventCaster.getInstance();
    private final double           topElement    = 0.6;
    private boolean                firstEntrance = false;

    //
    private final ProjectData mProject;

    //
    private final SceneFlowEditor   mSceneFlowEditor;
    private final ScriptEditorPanel mSceneDocEditor;
    private long                    previousTime;

    /**
     *
     *
     *
     *
     *
     */
    public ProjectEditor(ProjectData project) {
        super(JSplitPane.VERTICAL_SPLIT, true);
        mProject        = project;
        mSceneDocEditor = new ScriptEditorPanel(mProject.getSceneScript(), mProject.getSceneFlow(),
                mProject.getPreferences(), mProject.getPreferencesFileName(), this);
        mSceneFlowEditor = new SceneFlowEditor(mProject.getSceneFlow(), mProject, mSceneDocEditor);
        mObservable.addObserver(mSceneFlowEditor);
        mObservable.addObserver(mSceneDocEditor);
        mEventCaster.append(this);

        NodeSelectedEvent e = new NodeSelectedEvent(this, mProject.getSceneFlow());

        EventCaster.getInstance().convey(e);

        WorkSpaceSelectedEvent ev = new WorkSpaceSelectedEvent(this);

        EventCaster.getInstance().convey(ev);
        initComponents();
    }

    @Override
    public void update(java.util.Observable obs, Object obj) {

        // mLogger.message("ProjectEditor.update(" + obj + ")");
        mObservable.update(obj);
    }

    /**
     *
     *
     *
     *
     *
     */
    @Override
    public void update(EventObject evt) {

        // System.out.println(evt.getClass());
        if ((evt instanceof FunctionSelectedEvent) || (evt instanceof TreeEntrySelectedEvent)) {
            {
                showSceneDocEditor();
            }
        }

        if ((evt instanceof NodeSelectedEvent) &&!mSceneDocEditor.isPinPricked()) {
            hideSceneDocEditor();
        }
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
     *
     *
     *
     *
     *
     */
    public void close() {
        if (mProject.hasChanged()) {
            int response = JOptionPane.showConfirmDialog(this,
                               "The project \"" + mProject.getProjectName() + "\" has changed.  Save it?",
                               "Save before quitting?", JOptionPane.YES_NO_OPTION);

            if (response == JOptionPane.YES_OPTION) {
                save();
            } else if (response == JOptionPane.CANCEL_OPTION) {}
            else if (response == JOptionPane.NO_OPTION) {}
            else {}
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
            JOptionPane.showMessageDialog(this, "Wrong Scene Script Syntax.", "Cannot Format Scene Script.",
                                          JOptionPane.ERROR_MESSAGE);
        }
    }

    /**
     *
     *
     *
     *
     *
     */
    private void initComponents() {

        //
        setBorder(BorderFactory.createEmptyBorder());
        setBackground(Color.WHITE);
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

        // ProjectEditor thisPE = this;
        setUI(new BasicSplitPaneUI() {
            @Override
            public BasicSplitPaneDivider createDefaultDivider() {
                return new BasicSplitPaneDivider(this) {
                    @Override
                    public void setBorder(Border b) {}
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

                    /**
                     * Shows the bottom part of the editor when mouse goes over the border
                     * @param me
                     */
                    @Override
                    protected void processMouseMotionEvent(MouseEvent me) {
                        super.processMouseMotionEvent(me);

                        switch (me.getID()) {
                        case MouseEvent.MOUSE_MOVED :
                            showSceneDocEditor();
                        }
                    }
                };
            }
        });

//      Toolkit.getDefaultToolkit().addAWTEventListener(new AWTEventListener() {
//          public void eventDispatched(AWTEvent event) {
//              if(event instanceof MouseEvent){
//                  MouseEvent evt = (MouseEvent)event;
//                  /*System.out.println(evt.getID()+"    "+topElement*java.awt.Toolkit.getDefaultToolkit().getScreenSize().getHeight()+ "     "+ Editor.getInstance().getHeight() 
//                  + "     " + evt.getComponent() + "        " + evt.getXOnScreen() );*/
//                  System.out.println(evt.getComponent());
//                  if(evt.getComponent() instanceof de.dfki.vsm.editor.ElementEditor || 
//                     evt.getComponent() instanceof de.dfki.vsm.editor.WorkSpace) {
//                      if(firstEntrance){
//                          firstEntrance = false;
//                          previousTime = System.currentTimeMillis();
//                      }
//                      double waiting = java.util.concurrent.TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()-previousTime);
//                      //System.out.println(waiting);
//                      if(waiting >= 2){
//                          hideSceneDocEditor();
//                      }
//                  }
//              }
//          }
//      }, AWTEvent.MOUSE_EVENT_MASK);
        setDividerSize(10);
        setContinuousLayout(true);
        setTopComponent(mSceneFlowEditor);
        setBottomComponent(mSceneDocEditor);

        // setting size
        boolean showSceneFlowEditor = Boolean.valueOf(Preferences.getProperty("showscenefloweditor"));
        boolean showSceneDocEditor  = Boolean.valueOf(Preferences.getProperty("showsceneeditor"));

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
            public void componentMoved(ComponentEvent e) {}
            @Override
            public void componentShown(ComponentEvent e) {}
            @Override
            public void componentHidden(ComponentEvent e) {}
        });
    }

    /**
     * Shows the bottom part of the project editor
     */
    public void showSceneDocEditor() {
        int originalPos = (int) (topElement * getHeight());

        this.setDividerLocation(originalPos);
        firstEntrance = true;
    }

    /*
     * Hides the bottom part of the project editor
     */
    public void hideSceneDocEditor() {
        this.setDividerLocation(this.getHeight());
    }

    /**
     *
     *
     *
     *
     *
     */
    private class Observable extends java.util.Observable {
        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
}
