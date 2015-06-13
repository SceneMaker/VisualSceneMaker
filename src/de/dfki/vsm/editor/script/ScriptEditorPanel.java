package de.dfki.vsm.editor.script;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.FunctionEditor;
import de.dfki.vsm.editor.ProjectEditor;
import de.dfki.vsm.editor.SceneElementDisplay;
import de.dfki.vsm.editor.event.SceneSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.syn.SyntaxDocument;

import org.ujmp.core.collections.ArrayIndexList;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.StyledDocument;

/**
 * @author Gregor Mehlmann
 */
public final class ScriptEditorPanel extends JPanel implements DocumentListener, EventListener, Observer {

    // The System Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Event Caster
    private final EventCaster mEventCaster = EventCaster.getInstance();

    // The Observable Part
    private final Noticeable mNoticeable = new Noticeable();

    // The Script Editor Pane
    private final JScrollPane         mScrollPane;
    private final ScriptToolBar       mScenesToolbar;
    private final JTabbedPane         mTabPane;
    private final ScriptEditorPane    mEditorPane;
    private final CaretStatusLabel    mStatusLabel;
    private final SceneElementDisplay mElementPane;
    private final SceneScript         mSceneScript;
    private final FunctionEditor      mFunctionEditor;
    private final DialogActEditor     mDialogActEditor;
    private final ProjectPreferences  mPreferences;
    private final String              mPreferencesFileName;
    private ProjectEditor             mParentPE;    // CONTAINER PROJECT EDITOR
    private ArrayList<Integer>        searchOffsets;
    private String                    lastSearchedScene;
    private int                       lastIndex;
    Highlighter.HighlightPainter      painter;
    private int tabCounter = 1;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ScriptEditorPanel(final SceneScript script, final SceneFlow sceneflow, final ProjectPreferences preferences,
                             final String preferencesFileName, final ProjectEditor parentPE) {

        // Parent project editor
        mParentPE = parentPE;

        // Initialize The Scene Script
        mSceneScript = script;

        // Grab project preferences
        mPreferences         = preferences;
        mPreferencesFileName = preferencesFileName;

        // Initialize The Status Label
        mStatusLabel = new CaretStatusLabel("");

        // Initialize The Editor Pane
        mEditorPane = new ScriptEditorPane(mPreferences);
        mEditorPane.addCaretListener(mStatusLabel);
        mEditorPane.getDocument().addDocumentListener(this);
        
        
        
        // Initialize The Scroll Pane
        mScrollPane = new JScrollPane(mEditorPane);
        mScrollPane.setBorder(BorderFactory.createEtchedBorder());

        // Initialize The Function Definition Panel
        mFunctionEditor = new FunctionEditor(sceneflow);

        // Initialize The Dialog Act Panel
        mDialogActEditor = new DialogActEditor(mParentPE.getProject());

        // Initialize Tabbed Pane
        mTabPane = new JTabbedPane();

        addTab("Script        ", mScrollPane);
        addTab("Functions     ", mFunctionEditor);
        addTab("DialogAct [Experimental]", mDialogActEditor);

        
        // Initialize the Toolbar
        mScenesToolbar = new ScriptToolBar(this);

        // Initialize The Scroll Pane
        mElementPane = new SceneElementDisplay();
        mElementPane.setVisible(Boolean.valueOf(mPreferences.getProperty("showsceneelements")));
        mNoticeable.addObserver(mElementPane);
        mNoticeable.addObserver(mEditorPane);

        // Initialize The Components
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder());
        setBackground(Color.WHITE);
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

        searchOffsets     = new ArrayList<Integer>();
        lastSearchedScene = "";
        lastIndex         = 0;

        Highlighter highlighter = new DefaultHighlighter();

        mEditorPane.setHighlighter(highlighter);
        painter = new DefaultHighlighter.DefaultHighlightPainter(Preferences.sHIGHLIGHT_SCENE_COLOR);
        
    }
    
    void addTab(String tabName, final JComponent content) {
        JEditorPane ep = new JEditorPane();
        ep.setEditable(false);
        mTabPane.addTab(null, new JScrollPane(ep));
        JLabel tabLabel = new JLabel(tabName);
       // Create an AddButton
        final AddButton mAddButton = new AddButton();
        mAddButton.setTabPos(tabCounter-1);
        mAddButton.removeMouseListener(mAddButton.getMouseListeners()[1]);
        mAddButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(MouseEvent me) {
                if (mTabPane.getSelectedIndex()==mAddButton.getTabPos()) {
                    mAddButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add_blue.png"));
                }             
            }
            public void mouseExited(MouseEvent me) {
                mAddButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add.png"));
            }
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                if (mTabPane.getSelectedIndex()==mAddButton.getTabPos()) {
                    if (content instanceof FunctionEditor)
                    {
                        ((FunctionEditor)content).addNewFunction();
                    }
                    if (content instanceof JScrollPane)
                    {
                        mEditorPane.append("scene_@@ SceneName:\n" + "character: Text.\n\n"); 
                        mEditorPane.requestFocusInWindow();
                      
                    }
                    if (content instanceof DialogActEditor)
                    {
                        //PLUS ACTION FOR DIALGOACTEDITOR
                    }
                    
                }
            }
        });
        if (tabCounter != 0) {
          JPanel pnl = new JPanel();
          pnl.setOpaque(false);
          pnl.add(tabLabel);
          pnl.add(mAddButton);
          
          mTabPane.setTabComponentAt(mTabPane.getTabCount() - 1, pnl);
          mTabPane.setComponentAt(mTabPane.getTabCount() - 1, content);
          mTabPane.setSelectedIndex(mTabPane.getTabCount() - 1);
        }

        tabCounter++;
      }
    
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final Observable obs, final Object obj) {

        // Notify All Observers
        mNoticeable.notify(obj);
    }

    /**
     * Function to know if the panel can be hidden
     *
     * @return boolean
     */
    public boolean isPinPricked() {
        return mScenesToolbar.isPinPricked();
    }

    /**
     * Fixes the pin
     *
     * @return
     */
    public void prickPin() {
        mScenesToolbar.prickPin();
    }

    public JTabbedPane getTabPane() {
        return mTabPane;
    }

    public ProjectPreferences getPreferences() {
        return mPreferences;
    }

    public String getPreferencesFileName() {
        return mPreferencesFileName;
    }

    // **********************************
    // Get and set parent project editor
    public ProjectEditor getmParentPE() {
        return mParentPE;
    }

    public void setmParentPE(ProjectEditor mParentPE) {
        this.mParentPE = mParentPE;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final EventObject event) {

        /*
         * try {
         *
         * ///////////////////////////////////////////////////////////////////
         * if (event instanceof GroupSelectedEvent) {
         * /
         * SM3SceneGroup group = ((GroupSelectedEvent) event).getGroup();
         * // Compute the set of rectangles of the whitelisted scenes
         * // from that group and schedule the tasks to highlight them
         * for (SM3SceneObject scene : group.getWhiteList()) {
         * Rectangle sceneRect
         * = mTextSpace.modelToView(scene.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(scene.getPosition().mEndPos));
         * HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_GREEN_COLOR, sceneRect);
         * mVisualisationTasks.add(highlightTask);
         * mVisualisationTimer.schedule(highlightTask, 0, 25);
         *
         * }
         * // Compute the set of rectangles of the blacklisted scenes
         * // from that group and schedule the tasks to highlight them
         * for (SM3SceneObject scene : group.getBlackList()) {
         * Rectangle sceneRect
         * = mTextSpace.modelToView(scene.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(scene.getPosition().mEndPos));
         * HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_YELLOW_COLOR, sceneRect);
         * mVisualisationTasks.add(highlightTask);
         * mVisualisationTimer.schedule(highlightTask, 0, 25);
         *
         * }
         * }
         * ///////////////////////////////////////////////////////////////////
         * if (event instanceof SceneExecutedEvent) {
         * SM3SceneObject scene = ((SceneExecutedEvent) event).getScene();
         * // Clear the vector of tasks to stop the highlight pane visualizing
         * // the tasks that are still in the timer's queue. TODO: Cancel the
         * // tasks from the timer queue, so that the xext time this list is not
         * // empty there will not be visualized "old" tasks!!!
         * mVisualisationTasks.clear();
         * // Get the position of the scene header in the document and
         * // compute the rectangle that has to be highlighted in the document
         * Rectangle sceneRect
         * = mTextSpace.modelToView(scene.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(scene.getPosition().mEndPos));
         * for (SM3SceneTurn turn : scene.getBody()) {
         * Rectangle turnRect
         * = mTextSpace.modelToView(turn.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(turn.getPosition().mEndPos));
         * sceneRect.add(turnRect);
         * }
         * // Set the caret position
         * mTextSpace.setCaretPosition(scene.getPosition().mBeginPos);
         * // Schedule the highlight task for that rectangle
         * HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_GREEN_COLOR, sceneRect);
         * mVisualisationTasks.add(highlightTask);
         * mVisualisationTimer.schedule(highlightTask, 0, 25);
         * // TODO: make visualization dependen on lenght of text  turnText().length()
         *
         * }
         * ///////////////////////////////////////////////////////////////////
         * if (event instanceof TurnExecutedEvent) {
         * SM3SceneTurn turn = ((TurnExecutedEvent) event).getTurn();
         * // Get the position of the turn  in the document and
         * // Compute the rectangle that has to be highlighted in the document
         * Rectangle wordRect
         * = mTextSpace.modelToView(turn.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(turn.getPosition().mEndPos));
         * // Set the caret position
         * mTextSpace.setCaretPosition(turn.getPosition().mBeginPos);
         * // Schedule the highlight task for that rectangle
         * HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_RED_COLOR, wordRect);
         * mVisualisationTasks.add(highlightTask);
         * mVisualisationTimer.schedule(highlightTask, 0, 25 );
         * // TODO: make visualization dependen on lenght of text  turnText().length()
         * }
         * ///////////////////////////////////////////////////////////////////
         * if (event instanceof UtteranceExecutedEvent) {
         * SM3SceneUttr utt = ((UtteranceExecutedEvent) event).getUtterance();
         * // Get the position of the turn  in the document and
         * // Compute the rectangle that has to be highlighted in the document
         * Rectangle uttRect
         * = mTextSpace.modelToView(utt.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(utt.getPosition().mEndPos));
         * // Set the caret position
         * mTextSpace.setCaretPosition(utt.getPosition().mBeginPos);
         * // Schedule the highlight task for that rectangle
         * HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_RED_COLOR, uttRect);
         * mVisualisationTasks.add(highlightTask);
         * mVisualisationTimer.schedule(highlightTask, 0, 25);
         * // TODO: make visualization dependen on lenght of text  turnText().length()
         * }
         * ///////////////////////////////////////////////////////////////////
         * if (event instanceof WordExecutedEvent) {
         * SM3SceneWord word = ((WordExecutedEvent) event).getWord();
         * // Get the position of the word in the document and
         * // compute the rectangle that has to be highlighted in the document
         * Rectangle wordRect
         * = mTextSpace.modelToView(word.getPosition().mBeginPos).union(
         * mTextSpace.modelToView(word.getPosition().mEndPos));
         * // Set the caret position
         * mTextSpace.setCaretPosition(word.getPosition().mBeginPos);
         * // Schedule the highlight task for that rectangle
         * HighlightTask highlightTask = new HighlightTask(100, this, Preferences.sTRANSLUCENT_BLUE_COLOR, wordRect);
         * mVisualisationTasks.add(highlightTask);
         * mVisualisationTimer.schedule(highlightTask, 0, 25 );
         * // TODO: make visualization dependen on lenght of text  turnText().length()
         * }
         * } catch (BadLocationException e) {
         * e.printStackTrace();
         * }
         */
        if (event instanceof SceneSelectedEvent) {
            String sg       = ((SceneSelectedEvent) event).getGroup().getName().trim();
            String language = ((SceneSelectedEvent) event).getLanguage();

            // System.out.println("Language selected: " + language);
            // This indicates user clicked the same scene name, advance to next
            // search offset.
            if (lastSearchedScene.equals("scene_" + language + " " + sg)) {
                advanceToNextSearchOffset();
            }    // Different search is required, perform offset recalculation.
                    else {
                search(sg, language, mEditorPane, painter);
            }

            if (searchOffsets.size() > 0) {
                try {
                    mEditorPane.requestFocus();
                    mEditorPane.setCaretPosition(searchOffsets.get(lastIndex) + sg.length() + 10);
                    mEditorPane.scrollRectToVisible(mEditorPane.modelToView(searchOffsets.get(lastIndex)));
                } catch (BadLocationException e) {
                    System.out.println("" + e);
                }
            }
        }

        if (event instanceof TreeEntrySelectedEvent) {
            if (((TreeEntrySelectedEvent) event).getmEntry().getText().contains("Scenes")) {
                mTabPane.setSelectedComponent(mScrollPane);
            } else if (((TreeEntrySelectedEvent) event).getmEntry().getText().contains("Functions")) {
                mTabPane.setSelectedComponent(mFunctionEditor);
            } else if (((TreeEntrySelectedEvent) event).getmEntry().getText().contains("Dialog")) {
                mTabPane.setSelectedComponent(mDialogActEditor);
            }
        }
    }

    public void advanceToNextSearchOffset() {
        if (lastIndex == searchOffsets.size() - 1) {
            lastIndex = 0;
        } else {
            lastIndex++;
        }
    }

    /* Search for a scene name and give the position of the selected text */
    public void search(String word, String language, JTextComponent comp, Highlighter.HighlightPainter painter) {
        this.lastIndex         = 0;
        this.lastSearchedScene = "scene_" + language + " " + word;
        this.searchOffsets     = new ArrayIndexList<Integer>();

        Highlighter highlighter = comp.getHighlighter();

        // Remove any existing highlights for last word
        Highlighter.Highlight[] highlights = highlighter.getHighlights();

        for (int i = 0; i < highlights.length; i++) {
            Highlighter.Highlight h = highlights[i];

            if (h.getPainter() instanceof DefaultHighlighter.DefaultHighlightPainter) {
                highlighter.removeHighlight(h);
            }
        }

        String content = null;

        try {
            Document d = comp.getDocument();

            content = d.getText(0, d.getLength()).toLowerCase();
        } catch (BadLocationException e) {

            // Cannot happen
        }

        word = word.toLowerCase();

        int lastLocalIndex = 0;
        int wordSize       = word.length();

        while ((lastLocalIndex = content.indexOf("scene_" + language + " " + word, lastLocalIndex)) != -1) {
            lastLocalIndex += 9;

            int endIndex     = lastLocalIndex + wordSize;
            int limiterIndex = content.indexOf(':', lastLocalIndex);

            try {
                if ((lastLocalIndex + endIndex) == (lastLocalIndex + limiterIndex)) {
                    highlighter.addHighlight(lastLocalIndex, endIndex, painter);
                    searchOffsets.add(lastLocalIndex);
                }
            } catch (BadLocationException e) {

                // Nothing to do
            }

            lastLocalIndex = endIndex;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void changedUpdate(final DocumentEvent event) {

        // parse(event);
        // mLogger.message("changedUpdate");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void insertUpdate(final DocumentEvent event) {
        parse(event);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void removeUpdate(final DocumentEvent event) {
        parse(event);
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

                            if (caretCoords != null) {
                                setText(dot + " : [" + caretCoords.x + ", " + caretCoords.y + "]" + "\r\n");
                            }
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


    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private class Noticeable extends Observable {
        public void notify(final Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
}
