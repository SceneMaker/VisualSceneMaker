package de.dfki.vsm.editor.script;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.SceneElementDisplay;
import de.dfki.vsm.editor.FunctionEditor;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.syn.SyntaxDocument;
import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

/**
 * @author Gregor Mehlmann
 */
public final class ScriptEditorPanel extends JPanel
        implements DocumentListener, EventListener, Observer {

    // The System Logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The Event Caster
    private final EventCaster mEventCaster
            = EventCaster.getInstance();
    // The Observable Part
    private final Noticeable mNoticeable
            = new Noticeable();
    // The Script Editor Pane
    private final JScrollPane mScrollPane;
    private final ScriptToolBar mScenesToolbar;
    private final JTabbedPane mTabPane;
    private final ScriptEditorPane mEditorPane;
    private final CaretStatusLabel mStatusLabel;
    private final SceneElementDisplay mElementPane;

    private final SceneScript mSceneScript;
    private final FunctionEditor mFunctionEditor;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private class Noticeable extends Observable {

        public void notify(final Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final Observable obs, final Object obj) {
        // Notify All Observers
        mNoticeable.notify(obj);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ScriptEditorPanel(final SceneScript script, final SceneFlow sceneflow) {
        // Initialize The Scene Script
        mSceneScript = script;
        // Initialize The Status Label
        mStatusLabel = new CaretStatusLabel("");	
		
        // Initialize The Editor Pane
        mEditorPane = new ScriptEditorPane();
        mEditorPane.addCaretListener(mStatusLabel);
        mEditorPane.getDocument().addDocumentListener(this);
		
        // Initialize The Scroll Pane
        mScrollPane = new JScrollPane(mEditorPane);
        mScrollPane.setBorder(BorderFactory.createEtchedBorder());
        
        // Initialize The Function Definition Panel
        mFunctionEditor = new FunctionEditor(sceneflow);
        	
        // Initialize Tabbed Pane
        mTabPane = new JTabbedPane(); 
        mTabPane.add("Script Editor", mScrollPane);  
        mTabPane.add("Function Editor", mFunctionEditor);
		
       
        // Initialize the Toolbar
        mScenesToolbar  = new ScriptToolBar(this);
        // Initialize The Scroll Pane
        mElementPane = new SceneElementDisplay();
        mNoticeable.addObserver(mElementPane);
        mNoticeable.addObserver(mEditorPane);
        // Initialize The Components
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder());
        add(mScenesToolbar, BorderLayout.NORTH);
        add(mElementPane, BorderLayout.WEST);
        add(mTabPane, BorderLayout.CENTER);
        add(mStatusLabel, BorderLayout.SOUTH);
        // Register As Event Listener
        mEventCaster.append(this);
        //
        try {
            mEditorPane.getDocument().insertString(0, mSceneScript.getText(), null);
        } catch (BadLocationException exc) {
            exc.printStackTrace();
        }

    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final EventObject event) {
        /*
         try {
          
         ////////////////////////////////////////////////////////////////////
         if (event instanceof GroupSelectedEvent) {
         //
         SM3SceneGroup group = ((GroupSelectedEvent) event).getGroup();
         // Compute the set of rectangles of the whitelisted scenes
         // from that group and schedule the tasks to highlight them
         for (SM3SceneObject scene : group.getWhiteList()) {
         Rectangle sceneRect
         = mTextSpace.modelToView(scene.getPosition().mBeginPos).union(
         mTextSpace.modelToView(scene.getPosition().mEndPos));
         HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_GREEN_COLOR, sceneRect);
         mVisualisationTasks.add(highlightTask);
         mVisualisationTimer.schedule(highlightTask, 0, 25);

         }
         // Compute the set of rectangles of the blacklisted scenes
         // from that group and schedule the tasks to highlight them
         for (SM3SceneObject scene : group.getBlackList()) {
         Rectangle sceneRect
         = mTextSpace.modelToView(scene.getPosition().mBeginPos).union(
         mTextSpace.modelToView(scene.getPosition().mEndPos));
         HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_YELLOW_COLOR, sceneRect);
         mVisualisationTasks.add(highlightTask);
         mVisualisationTimer.schedule(highlightTask, 0, 25);

         }
         }
         ////////////////////////////////////////////////////////////////////
         if (event instanceof SceneExecutedEvent) {
         SM3SceneObject scene = ((SceneExecutedEvent) event).getScene();
         // Clear the vector of tasks to stop the highlight pane visualizing
         // the tasks that are still in the timer's queue. TODO: Cancel the
         // tasks from the timer queue, so that the xext time this list is not
         // empty there will not be visualized "old" tasks!!!
         mVisualisationTasks.clear();
         // Get the position of the scene header in the document and
         // compute the rectangle that has to be highlighted in the document
         Rectangle sceneRect
         = mTextSpace.modelToView(scene.getPosition().mBeginPos).union(
         mTextSpace.modelToView(scene.getPosition().mEndPos));
         for (SM3SceneTurn turn : scene.getBody()) {
         Rectangle turnRect
         = mTextSpace.modelToView(turn.getPosition().mBeginPos).union(
         mTextSpace.modelToView(turn.getPosition().mEndPos));
         sceneRect.add(turnRect);
         }
         // Set the caret position
         mTextSpace.setCaretPosition(scene.getPosition().mBeginPos);
         // Schedule the highlight task for that rectangle
         HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_GREEN_COLOR, sceneRect);
         mVisualisationTasks.add(highlightTask);
         mVisualisationTimer.schedule(highlightTask, 0, 25);
         // TODO: make visualization dependen on lenght of text  turnText().length()

         }
         ////////////////////////////////////////////////////////////////////
         if (event instanceof TurnExecutedEvent) {
         SM3SceneTurn turn = ((TurnExecutedEvent) event).getTurn();
         // Get the position of the turn  in the document and
         // Compute the rectangle that has to be highlighted in the document
         Rectangle wordRect
         = mTextSpace.modelToView(turn.getPosition().mBeginPos).union(
         mTextSpace.modelToView(turn.getPosition().mEndPos));
         // Set the caret position
         mTextSpace.setCaretPosition(turn.getPosition().mBeginPos);
         // Schedule the highlight task for that rectangle
         HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_RED_COLOR, wordRect);
         mVisualisationTasks.add(highlightTask);
         mVisualisationTimer.schedule(highlightTask, 0, 25 );
         // TODO: make visualization dependen on lenght of text  turnText().length()
         }
         ////////////////////////////////////////////////////////////////////
         if (event instanceof UtteranceExecutedEvent) {
         SM3SceneUttr utt = ((UtteranceExecutedEvent) event).getUtterance();
         // Get the position of the turn  in the document and
         // Compute the rectangle that has to be highlighted in the document
         Rectangle uttRect
         = mTextSpace.modelToView(utt.getPosition().mBeginPos).union(
         mTextSpace.modelToView(utt.getPosition().mEndPos));
         // Set the caret position
         mTextSpace.setCaretPosition(utt.getPosition().mBeginPos);
         // Schedule the highlight task for that rectangle
         HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_RED_COLOR, uttRect);
         mVisualisationTasks.add(highlightTask);
         mVisualisationTimer.schedule(highlightTask, 0, 25);
         // TODO: make visualization dependen on lenght of text  turnText().length()
         }
         ////////////////////////////////////////////////////////////////////
         if (event instanceof WordExecutedEvent) {
         SM3SceneWord word = ((WordExecutedEvent) event).getWord();
         // Get the position of the word in the document and
         // compute the rectangle that has to be highlighted in the document
         Rectangle wordRect
         = mTextSpace.modelToView(word.getPosition().mBeginPos).union(
         mTextSpace.modelToView(word.getPosition().mEndPos));
         // Set the caret position
         mTextSpace.setCaretPosition(word.getPosition().mBeginPos);
         // Schedule the highlight task for that rectangle
         HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_BLUE_COLOR, wordRect);
         mVisualisationTasks.add(highlightTask);
         mVisualisationTimer.schedule(highlightTask, 0, 25 );
         // TODO: make visualization dependen on lenght of text  turnText().length()
         }
         } catch (BadLocationException e) {
         e.printStackTrace();
         }*/
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void changedUpdate(final DocumentEvent event) {
        //parse(event);
        //mLogger.message("changedUpdate");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void insertUpdate(final DocumentEvent event) {
        parse(event);
        //mLogger.message("insertUpdate");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void removeUpdate(final DocumentEvent event) {
        parse(event);
        //mLogger.message("removeUpdate");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void parse(final DocumentEvent event) {
        try {
            // Get The Syntax Document 
            final SyntaxDocument document = (SyntaxDocument) event.getDocument();
            // Get The Document Text
            final String text = document.getText(0, document.getLength());
            // Parse The Scene Script
            mSceneScript.parseTXT(text);
            // Update The Editor UI
            Editor.getInstance().update();
        } catch (Exception exc) {
            // Catch Error Or Exception
            mLogger.failure(exc.toString());
            exc.printStackTrace();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public boolean showElementDisplay() {
        if (mElementPane.isVisible()) {
            mElementPane.setVisible(false);
            return false;
        } else {
            mElementPane.setVisible(true);
            return true;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void close() {
        // Remove As Event Listener
        mEventCaster.remove(this);
        // Remove All Observers
        mNoticeable.deleteObservers();
        // Remove Caret Listener
        mEditorPane.removeCaretListener(mStatusLabel);
        // TODO: Stop The Document Painter Timer
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private class CaretStatusLabel extends JLabel implements CaretListener {

        public CaretStatusLabel(String label) {
            super(label);
        }

        @Override
        public void caretUpdate(CaretEvent e) {
            displaySelectionInfo(e.getDot(), e.getMark());
        }

        protected void displaySelectionInfo(final int dot, final int mark) {
            SwingUtilities.invokeLater(new Runnable() {

                @Override
                public void run() {
                    if (dot == mark) {
                        try {
                            Rectangle caretCoords = mEditorPane.modelToView(dot);
                            setText(dot + " : [" + caretCoords.x + ", " + caretCoords.y + "]" + "\r\n");
                        } catch (BadLocationException ble) {
                            setText(dot + "\r\n");
                        }
                    } else if (dot < mark) {
                        setText("[" + dot + " - " + mark + "]" + "\r\n");
                    } else {
                        setText("[" + mark + " - " + dot + "]" + "\r\n");
                    }
                }
            });
        }
    }

}
