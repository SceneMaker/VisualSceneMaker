package de.dfki.vsm.editor;

//package de.dfki.vsm.editor;
//
//import de.dfki.vsm.mod.Project;
//import de.dfki.vsm.util.event.Event;
//import de.dfki.vsm.util.event.EventListener;
//import de.dfki.vsm.util.event.EventMulticaster;
//import de.dfki.vsm.util.log.Logger;
//import java.util.Observer;
//import javax.swing.BorderFactory;
//import javax.swing.ImageIcon;
//import javax.swing.JTabbedPane;
//
///**
// * @author Not me
// */
//public class ContentEditor extends JTabbedPane implements Observer, EventListener {
//
//    /**
//     * The project which is maintained by this editor
//     */
//    private final Project mProject;
//    /**
//     * The
//     */
//    private final SceneDocumentEditor mSceneDocumentEditor;
//    /**
//     * The
//     */
//    //private final GesticonEditor mGesticonEditor;
//    //
//    /**
//     * The observable of this editor
//     */
//    private final Observable mObservable = new Observable();
//    private final Logger mLogger = Logger.getInstance();
//    private final EventMulticaster mEventMulticaster = EventMulticaster.getInstance();
//
//    public class Observable extends java.util.Observable {
//
//        public void propagateToObservers(Object obj) {
//            mLogger.message(toString() + ".propagateToObservers(" + obj.toString() + ")");
//            setChanged();
//            notifyObservers(obj);
//        }
//    }
//
//    public ContentEditor(Project project) {
//        super(JTabbedPane.TOP, JTabbedPane.SCROLL_TAB_LAYOUT);
//        mProject = project;
//        mSceneDocumentEditor = new SceneDocumentEditor(mProject.getSceneDocument());
//       // mGesticonEditor = new GesticonEditor(mProject.getGesticon());
//        // Add observers
//        mObservable.addObserver(mSceneDocumentEditor);
//       // mObservable.addObserver(mGesticonEditor);
//        // Add event listeners
//        mEventMulticaster.add(mSceneDocumentEditor);
//       // mEventMulticaster.add(mGesticonEditor);
//        // Init components
//        initComponents();
//        setBorder(BorderFactory.createEmptyBorder());
//    }
//
//    public SceneDocumentEditor getSceneDocumentEditor() {
//        return mSceneDocumentEditor;
//    }
//
//    //public GesticonEditor getGesticonEditor() {
//   //     return mGesticonEditor;
//   // }
//
//    public Project getProject() {
//        return mProject;
//    }
//
//    private void initComponents() {
//
//        addTab("Scene Editor", new ImageIcon("data/img/scene.png"), mSceneDocumentEditor, "Edit the scene script document");
//       // addTab("Gesticon Editor", new ImageIcon("data/img/gesticon.png"), mGesticonEditor, "Edit the gesticon document");
//    }
//
//    public void update(Event event) {
//    }
//
//    public void update(java.util.Observable o, Object arg) {
//        mObservable.propagateToObservers(arg);
//    }
//
//    public String getDescription() {
//        return toString();
//    }
//
//    public void redraw(boolean layout) {
//        //
//        mSceneDocumentEditor.redraw(layout);
//        //mGesticonEditor.redraw(layout);
//        //
//        repaint();
//        revalidate();
//    }
//}




