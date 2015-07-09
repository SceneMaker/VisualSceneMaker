package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.util.Observer;

import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * @author Not me
 */
public class ProjectEditorList extends JTabbedPane implements EventListener, ChangeListener, Observer {

    private final Observable mObservable = new Observable();
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventMulticaster = EventCaster.getInstance();
    WorkSpace.ClipBoard previousCB = null;

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
        if (getSelectedEditorProject() != null) {
            mObservable.update(getSelectedEditorProject());
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

    public EditorProject getSelectedEditorProject() {
        if (getSelectedComponent() != null) {
            return ((ProjectEditor) getSelectedComponent()).getEditorProject();
        } else {
            return null;
        }
    }

    
    
   
    
    // Add a new project editor for a project
    public void append(final EditorProject project) {
        // Create a new project editor from project
        final ProjectEditor projectEditor = new ProjectEditor(project);
        // Add the new project editor as observer
        mObservable.addObserver(projectEditor);
        // Add the project editor to list of project 
        // editors and select it in the tabbed pane
        addTab(project.getProjectName(), projectEditor);
        setSelectedComponent(projectEditor);
    }

    // Remove a  project editor of a project
    public void closeProject() {
        final ProjectEditor projectEditor = getSelectedProjectEditor();
        //
        mObservable.deleteObserver(projectEditor);
        //
        projectEditor.close();
        //
        remove(projectEditor);
    }

    
    
    
    // Save the current editor project
    public final boolean save() {
        // Get the current project editor
        final ProjectEditor editor = (ProjectEditor) getSelectedComponent();
        // TODO: Can we couple this with the pending stuff?
        setTitleAt(getSelectedIndex(), getTitleAt(getSelectedIndex()).replace("*", ""));
        // Save the current editor project      
        return editor.save();

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

    // Check if the editor list is empty
    public final boolean isEmpty() {
        return (getTabCount() == 0);
    }
}
