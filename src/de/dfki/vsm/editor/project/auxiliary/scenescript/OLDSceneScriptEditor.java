package de.dfki.vsm.editor.project.auxiliary.scenescript;

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.auxiliary.functions.FunctionsEditor;
import de.dfki.vsm.editor.SceneElementDisplay;
import de.dfki.vsm.editor.event.SceneSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.editor.project.auxiliary.AuxiliaryToolBar;
import de.dfki.vsm.editor.project.auxiliary.dialogact.DialogActEditor;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.syn.SyntaxDocument;
import org.ujmp.core.collections.ArrayIndexList;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;

/**
 * @author Gregor Mehlmannn
 */
public final class OLDSceneScriptEditor extends JPanel implements DocumentListener, EventListener, Observer {

    // The system logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The event dispatcher instance
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();

    // The editor's observable 
    private final Observable mObservable = new Observable();

    private class Observable extends java.util.Observable {

        public void notify(final Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    // The Script Editor Pane
    private final JScrollPane mScrollPane;
    private final AuxiliaryToolBar mToolBar;
    private final JTabbedPane mTabPane;

    private final ScriptEditorPane mEditorPane;
    private final CaretStatusLabel mStatusLabel;
    private final SceneElementDisplay mElementPane;
    private final SceneScript mSceneScript;

    private final FunctionsEditor mFunctionEditor;
    private final DialogActEditor mDialogActEditor;
    private final EditorConfig mPreferences;
    //private final String              mPreferencesFileName;
    private ArrayList<Integer> searchOffsets;
    private String lastSearchedScene;
    private int lastIndex;
    Highlighter.HighlightPainter painter;
    private int tabCounter = 1;
    private JButton mGesticonButton;
    private final JSplitPane scriptSplitPane;

    // The current editor project
    private final EditorProject mProject;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public OLDSceneScriptEditor(final EditorProject project) {
        // Initialize the editor project
        mProject = project;
        // Initialize the scene script
        mSceneScript = mProject.getSceneScript();
        // Initialize the editor config
        mPreferences = mProject.getEditorConfig();
        // Initialize The Status Label
        mStatusLabel = new CaretStatusLabel("");

        // Initialize The Editor Pane
        mEditorPane = new ScriptEditorPane(mProject);
        mEditorPane.addCaretListener(mStatusLabel);
        mEditorPane.getDocument().addDocumentListener(this);

        // Initialize The Scroll Pane
        mScrollPane = new JScrollPane(mEditorPane);
        mScrollPane.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));

        // Initialize The Function Definition Panel
        mFunctionEditor = new FunctionsEditor(mProject);

        // Initialize The Dialog Act Panel
        mDialogActEditor = new DialogActEditor(mProject);

        // Initialize Tabbed Pane
        mTabPane = new JTabbedPane();
        // Initialize The Scroll Pane
        mElementPane = new SceneElementDisplay(mProject);
        mObservable.addObserver(mElementPane);
        mObservable.addObserver(mEditorPane);
        mGesticonButton = new JButton(Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png"));
        mGesticonButton.setRolloverIcon(Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
        mGesticonButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent me) {
                showElementDisplay();
            }
        });
        mGesticonButton.setContentAreaFilled(false);
        mGesticonButton.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        mGesticonButton.setFocusable(false);
        //
        Box bxTop = Box.createHorizontalBox();
        bxTop.add(mGesticonButton);
        bxTop.add(Box.createHorizontalGlue());
        //
        scriptSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        scriptSplitPane.setDividerSize(2);
        scriptSplitPane.setLeftComponent(mElementPane);
        scriptSplitPane.setRightComponent(mScrollPane);

        if (Boolean.valueOf(mPreferences.getProperty("showsceneelements"))) {
            scriptSplitPane.setDividerLocation(250);
        }
        if (!Boolean.valueOf(mPreferences.getProperty("showsceneelements"))) {
            scriptSplitPane.setDividerLocation(0);
        }
        Box bxBottom = Box.createHorizontalBox();
        bxBottom.add(scriptSplitPane);
        //Script Panel
        JPanel scriptTabPanel = new JPanel();
        scriptTabPanel.setLayout(new BoxLayout(scriptTabPanel, BoxLayout.Y_AXIS));
        scriptTabPanel.add(bxTop);
        scriptTabPanel.add(bxBottom);

        addTab("Script        ", scriptTabPanel);
        addTab("Functions     ", mFunctionEditor);
        addTab("DialogAct [Experimental]", mDialogActEditor);

        // Initialize the Toolbar
        mToolBar = new AuxiliaryToolBar(mProject);

        // Initialize The Components
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder());
        setBackground(Color.WHITE);
        add(mToolBar, BorderLayout.NORTH);
        add(mTabPane, BorderLayout.CENTER);
        add(mStatusLabel, BorderLayout.SOUTH);

        // Register As Event Listener
        mEventCaster.register(this);

        //
        try {
            mEditorPane.getDocument().insertString(0, mSceneScript.getText(), null);
        } catch (BadLocationException exc) {
            exc.printStackTrace();
        }

        searchOffsets = new ArrayList<Integer>();
        lastSearchedScene = "";
        lastIndex = 0;

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
        mAddButton.setTabPos(tabCounter - 1);
        mAddButton.removeMouseListener(mAddButton.getMouseListeners()[1]);
        mAddButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent me) {
                if (mTabPane.getSelectedIndex() == mAddButton.getTabPos()) {
                    mAddButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add_blue.png"));
                }
            }

            @Override
            public void mouseExited(MouseEvent me) {
                mAddButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add.png"));
            }

            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                if (mTabPane.getSelectedIndex() == mAddButton.getTabPos()) {
                    if (content instanceof FunctionsEditor) {
                        ((FunctionsEditor) content).addNewFunction();
                    }
                    if (content instanceof JPanel) {
                        mEditorPane.append("scene_@@ SceneName:\n" + "character: Text.\n\n");
                        mEditorPane.requestFocusInWindow();
                    }
                    if (content instanceof DialogActEditor) {
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

    // Get the pin pricked flag
    public boolean isPinPricked() {
        return mToolBar.isPinPricked();
    }

    // Set the pin pricked flag
    public void setPinPricked() {
        mToolBar.prickPin();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final EventObject event) {

        if (event instanceof SceneSelectedEvent) {
            String sg = ((SceneSelectedEvent) event).getGroup().getName().trim();
            String language = ((SceneSelectedEvent) event).getLanguage();

            // System.out.println("Language selected: " + language);
            // This indicates user clicked the same scene name, advance to next
            // search offset.
            if (lastSearchedScene.equals("scene_" + language + " " + sg)) {
                advanceToNextSearchOffset();
            } // Different search is required, perform offset recalculation.
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
        this.lastIndex = 0;
        this.lastSearchedScene = "scene_" + language + " " + word;
        this.searchOffsets = new ArrayIndexList<Integer>();

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
        int wordSize = word.length();

        while ((lastLocalIndex = content.indexOf("scene_" + language + " " + word, lastLocalIndex)) != -1) {
            lastLocalIndex += 9;

            int endIndex = lastLocalIndex + wordSize;
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
    // React to a change update of the document listener
    @Override
    public final void changedUpdate(final DocumentEvent event) {
        // Parse the scenescript whenever a change happened
        // parse(event);
    }

    // React to an insert update of the document listener
    @Override
    public final void insertUpdate(final DocumentEvent event) {
        // Parse the scenescript whenever an insert happened
        parse(event);
    }

    // React to a remove update of the document listener
    @Override
    public final void removeUpdate(final DocumentEvent event) {
        // Parse the scenescript whenever a remove happened
        parse(event);
    }

    // Parse the scenescript text in the document
    private boolean parse(final DocumentEvent event) {
        try {
            // Get the syntax document
            final SyntaxDocument document = (SyntaxDocument) event.getDocument();
            // Get the document text
            final String text = document.getText(0, document.getLength());
            // Parse the scenescript
            mSceneScript.parseTXT(text);
            // Update the editor UI because the
            // scenescript and thus the project
            // may have changed due to the parse
            // and these changes could result in
            // changes of the user interface in
            // various editor subcomponents
            EditorInstance.getInstance().update();
            // Print some information
            mLogger.message("Updating editor after successful parsinf the scenescript document");
            // Return true at success
            return true;
        } catch (final BadLocationException exc) {
            // Catch error or exception
            mLogger.failure(exc.toString());
            // Print an error message
            mLogger.failure("Error: Cannot parse the scenescript document");
            // Return false at error
            return false;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public void showElementDisplay() {

        if (Boolean.valueOf(mPreferences.getProperty("showsceneelements"))) {
            mGesticonButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less.png"));
            mPreferences.setProperty("showsceneelements", "false");
            //mPreferences.save(getPreferencesFileName());
            scriptSplitPane.setDividerLocation(0);
        } else {
            mGesticonButton.setIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more.png"));
            mPreferences.setProperty("showsceneelements", "true");
            //mPreferences.save(getPreferencesFileName());
            scriptSplitPane.setDividerLocation(250);
        }
        mGesticonButton.setRolloverIcon(Boolean.valueOf(mPreferences.getProperty("showsceneelements"))
                ? ResourceLoader.loadImageIcon("/res/img/toolbar_icons/more_blue.png")
                : ResourceLoader.loadImageIcon("/res/img/toolbar_icons/less_blue.png"));
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void close() {

        // Remove As Event Listener
        mEventCaster.remove(this);

        // Remove All Observers
        mObservable.deleteObservers();

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

    // Notify all observers
    @Override
    public void update(final java.util.Observable obs, final Object obj) {
        mObservable.notify(obj);
    }
}
