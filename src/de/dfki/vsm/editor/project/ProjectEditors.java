//package de.dfki.vsm.editor.project;
//
//import de.dfki.vsm.editor.WorkSpace;
//import de.dfki.vsm.model.sceneflow.Node;
//import de.dfki.vsm.util.evt.EventDispatcher;
//import de.dfki.vsm.util.evt.EventListener;
//import de.dfki.vsm.util.evt.EventObject;
//import java.util.Observer;
//import javax.swing.JTabbedPane;
//import javax.swing.event.ChangeEvent;
//import javax.swing.event.ChangeListener;
//
///**
// * @author Not me
// */
//public class ProjectEditors extends JTabbedPane implements EventListener, ChangeListener, Observer {
//
//    private final Observable mObservable = new Observable();
//    // private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
//    //private final EventDispatcher mEventMulticaster = EventDispatcher.getInstance();
// 
//
//    //
//    public ProjectEditors() {
//        super(JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
//        addChangeListener(this);
//        //mEventMulticaster.append(this);
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public void update(java.util.Observable obs, Object obj) {
//                mObservable.update(obj);
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    @Override
//    public void update(final EventObject event) {
//
//        // Do Nothing
//    }
//
//    
//    public ProjectEditor getProjectEditor() {
//        return (ProjectEditor) getSelectedComponent();
//    }
//
//    public EditorProject getEditorProject() {
//        if (getSelectedComponent() != null) {
//            return ((ProjectEditor) getSelectedComponent()).getEditorProject();
//        } else {
//            return null;
//        }
//    }
//
//    private class Observable extends java.util.Observable {
//
//        public void update(Object obj) {
//            setChanged();
//            notifyObservers(obj);
//        }
//    }
//
//    // Check if the editor list is empty
//    public final boolean isEmpty() {
//        return (getTabCount() == 0);
//    }
//}
