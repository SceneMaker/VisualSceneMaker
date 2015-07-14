package de.dfki.vsm.editor.project.auxiliary;

import de.dfki.vsm.editor.project.auxiliary.scenescript.OLDSceneScriptEditor;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.auxiliary.functions.FunctionsEditor;
import de.dfki.vsm.editor.project.auxiliary.dialogact.DialogActEditor;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.BorderLayout;
import java.awt.Color;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

/**
 * @author Gregor Mehlmannn
 */
public final class AuxiliaryEditor extends JPanel implements EventListener, Observer {

    // The system logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The event dispatcher instance
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();
    // The editor's observable part
    private final Observable mObservable = new Observable();
    // The tabbed pane of the editor
    private final JTabbedPane mEditorTabbedPane = new JTabbedPane();
    // The components of the edior
    private final AuxiliaryToolBar mAuxiliaryToolBar;
    private final FunctionsEditor mFunctionEditor;
    private final DialogActEditor mDialogActEditor;
    private final OLDSceneScriptEditor mSceneScriptEditor;
    // The current editor project
    private final EditorProject mProject;

    // Create the auxiliary editor
    public AuxiliaryEditor(final EditorProject project) {
        // Initialize the editor project
        mProject = project;
        // Initialize the auxiliary toolbar
        mAuxiliaryToolBar = new AuxiliaryToolBar(mProject);
        // Initialize the function editor
        mFunctionEditor = new FunctionsEditor(mProject);
        // Initialize the dialogact editor
        mDialogActEditor = new DialogActEditor(mProject);
        // Initialize the scenescript editor
        mSceneScriptEditor = new OLDSceneScriptEditor(mProject);
        // Initialize the observables
        mObservable.addObserver(mFunctionEditor);
        mObservable.addObserver(mDialogActEditor);
        mObservable.addObserver(mSceneScriptEditor);
        // Register as event listener
        mEventCaster.register(this);
        //
        // mEditorTabbedPane.addTab(, this);
        // Initialize the components
        setBackground(Color.WHITE);
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder());
        // Add the GUI components
        add(mAuxiliaryToolBar, BorderLayout.NORTH);
        add(mEditorTabbedPane, BorderLayout.CENTER);
        add(new JPanel(), BorderLayout.SOUTH);
    }

    // Get the pin pricked flag
    public boolean isPinPricked() {
        return mAuxiliaryToolBar.isPinPricked();
    }

    // Set the pin pricked flag
    public void setPinPricked() {
        mAuxiliaryToolBar.prickPin();
    }

    // Close the auxiliary editor
    public final void close() {
        // Remove as event listener
        mEventCaster.remove(this);
        // Remove all observers
        mObservable.deleteObservers();
        // Close all the editors
        mFunctionEditor.close();
        mDialogActEditor.close();
        mSceneScriptEditor.close();
    }

    // The observable class of the editor
    private final class Observable extends java.util.Observable {

        public void notify(final Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    // Update the observables of the editor
    @Override
    public final void update(final java.util.Observable obs, final Object obj) {
        mObservable.notify(obj);
    }

    // Update whenever an event has happened
    @Override
    public final void update(final EventObject event) {
        // Do Nothing
    }
}
