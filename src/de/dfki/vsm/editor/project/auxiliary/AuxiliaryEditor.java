//package de.dfki.vsm.editor.project.auxiliary;
//
//import de.dfki.vsm.editor.project.auxiliary.scenescript.OLDSceneScriptEditor;
//import de.dfki.vsm.editor.project.EditorProject;
//import de.dfki.vsm.editor.project.auxiliary.functions.FunctionsEditor;
//import de.dfki.vsm.editor.project.auxiliary.dialogact.DialogActEditor;
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//import java.awt.BorderLayout;
//import java.awt.Color;
//import javax.swing.BorderFactory;
//import javax.swing.JPanel;
//import javax.swing.JTabbedPane;
//
///**
// * @author Gregor Mehlmannn
// */
//public final class AuxiliaryEditor extends JPanel {
//
//    // The singelton logger instance
//    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
//    // The tabbed pane of the editor
//    private final JTabbedPane mEditorTabbedPane = new JTabbedPane();
//    private final FunctionsEditor mFunctionEditor;
//    private final DialogActEditor mDialogActEditor;
//    private final OLDSceneScriptEditor mSceneScriptEditor;
//
//    // The current editor project
//    private final EditorProject mProject;
//
//    // Create the auxiliary editor
//    public AuxiliaryEditor(final EditorProject project) {
//        // Initialize the editor project
//        mProject = project;
//        // Initialize the auxiliary toolbar
//        // Initialize the function editor
//        mFunctionEditor = new FunctionsEditor(mProject);
//        // Initialize the dialogact editor
//        mDialogActEditor = new DialogActEditor(mProject);
//        // Initialize the scenescript editor
//        mSceneScriptEditor = new OLDSceneScriptEditor(mProject);
//        // Initialize the components
//        setBackground(Color.WHITE);
//        setLayout(new BorderLayout());
//        setBorder(BorderFactory.createEmptyBorder());
//        // Add the GUI components
//        add(mEditorTabbedPane, BorderLayout.CENTER);
//        add(new JPanel(), BorderLayout.SOUTH);
//    }
//    // Close the auxiliary editor
//    public final void close() {
//         // Print some information
//        mLogger.message("Closing '" + this + "'");
//        // Close all the editors
//        mFunctionEditor.close();
//        mDialogActEditor.close();
//        mSceneScriptEditor.close();
//    }
//
//    // Refresh the visual appearance
//    public final void refresh() {
//        // Print some information
//        //mLogger.message("Refreshing '" + this + "'");
//        // Refresh the components
//        mFunctionEditor.refresh();
//        mDialogActEditor.refresh();
//        mSceneScriptEditor.refresh();
//    }
//}
