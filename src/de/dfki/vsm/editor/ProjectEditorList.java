package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;

import java.util.Observer;

import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * @author Gregor Mehlmann
 */
public class ProjectEditorList extends JTabbedPane implements EventListener, ChangeListener, Observer {
    private final Observable       mObservable       = new Observable();
    private final LOGDefaultLogger mLogger           = LOGDefaultLogger.getInstance();
    private final EventCaster      mEventMulticaster = EventCaster.getInstance();
    WorkSpace.ClipBoard            previousCB        = null;

    /**
     * 
     *
     *
     *
     *
     */
    public ProjectEditorList() {
        super(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
        addChangeListener(this);
        mEventMulticaster.append(this);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(java.util.Observable obs, Object obj) {
        mObservable.update(obj);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final EventObject event) {

        // Do Nothing
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void stateChanged(ChangeEvent e) {
        if (getSelectedProject() != null) {
            mObservable.update(getSelectedProject());
        }

        // copy and paste of nodes between the different projects
        ProjectEditor projectEditor = ((ProjectEditor) getSelectedComponent());

        if (projectEditor != null) {
            if (previousCB != null) {
                WorkSpace.ClipBoard currentCB = projectEditor.getSceneFlowEditor().getWorkSpace().getClipBoard();

                currentCB.clear();

                for (Node node : previousCB) {
                    currentCB.add(node);
                }
            }

            previousCB = projectEditor.getSceneFlowEditor().getWorkSpace().getClipBoard();
        }
    }

    public ProjectEditor getSelectedProjectEditor() {
        return (ProjectEditor) getSelectedComponent();
    }

    public ProjectData getSelectedProject() {
        if (getSelectedComponent() != null) {
            return ((ProjectEditor) getSelectedComponent()).getProject();
        } else {
            return null;
        }
    }

    public void add(ProjectData project) {

        // Create a new project editor from the given project
        ProjectEditor projectEditor = new ProjectEditor(project);

        // Add the new project editor as observer
        mObservable.addObserver(projectEditor);

        // Add the project editor to the list of project editors
        // and select it as a component in the tabbed pane
        addTab(project.getProjectName(), projectEditor);
        setSelectedComponent(projectEditor);

        // Initialize the workspace of the project editor's scene flow editor
        // TODO: push down into an init function
//      projectEditor.getSceneFlowEditor().getWorkSpace().showNodesOnWorkSpace();
//      projectEditor.getSceneFlowEditor().getWorkSpace().showEdgesOnWorkSpace();
//      projectEditor.getSceneFlowEditor().getWorkSpace().showVariablesOnWorkSpace(project.getSceneFlow());
//      projectEditor.getSceneFlowEditor().getWorkSpace().repaint();
//
//      // Revalidade and repaint
//      revalidate();
//      repaint();
    }

    public void saveCurrent() {
        ((ProjectEditor) getSelectedComponent()).save();
        setTitleAt(getSelectedIndex(), getTitleAt(getSelectedIndex()).replace("*", ""));
    }

    public void closeCurrent() {
        ProjectEditor projectEditor = ((ProjectEditor) getSelectedComponent());

        mObservable.deleteObserver(projectEditor);
        projectEditor.close();
        remove(projectEditor);
    }

    public void saveAll() {
        for (int i = 0; i < getTabCount(); i++) {
            ((ProjectEditor) getComponentAt(i)).save();
        }
    }

    public void closeAll() {

        // Delete all observers
        mObservable.deleteObservers();

        // Close all projects
        for (int i = 0; i < getTabCount(); i++) {
            ((ProjectEditor) getComponentAt(i)).close();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private class Observable extends java.util.Observable {
        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
}
