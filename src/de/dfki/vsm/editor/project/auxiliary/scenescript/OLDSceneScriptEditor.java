package de.dfki.vsm.editor.project.auxiliary.scenescript;

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.auxiliary.functions.FunctionsEditor;
import de.dfki.vsm.editor.SceneElementDisplay;
import de.dfki.vsm.editor.event.SceneSelectedEvent;
import de.dfki.vsm.editor.event.TreeEntrySelectedEvent;
import de.dfki.vsm.Preferences;
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
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.OverlayLayout;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.basic.BasicTabbedPaneUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Document;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;

/**
 * @author Gregor Mehlmannn
 */
public final class OLDSceneScriptEditor extends JPanel implements DocumentListener, EventListener {

    // The system logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The event dispatcher instance
    private final EventDispatcher mEventDispatcher = EventDispatcher.getInstance();

    // The Script Editor Pane
    private final JScrollPane mScrollPane;
    private final JTabbedPane mTabPane;

    private final ScriptEditorPane mEditorPane;
    private final CaretStatusLabel mStatusLabel;
    private final SceneElementDisplay mElementPane;

    private final FunctionsEditor mFunctionEditor;

    private final JPanel mScriptTabPanel = new JPanel();

    private ArrayList<Integer> searchOffsets;
    private String lastSearchedScene;
    private int lastIndex;
    Highlighter.HighlightPainter painter;
    private int tabCounter = 1;
    private JButton mGesticonButton;
    private final JSplitPane scriptSplitPane;

    private final JButton mPinButton;
    //PIN icons
    private final ImageIcon ICON_PIN_STANDARD = ResourceLoader.loadImageIcon("/res/img/pin.png");
    private final ImageIcon ICON_PIN_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/pin_blue.png");
    //PIN status
    private boolean pinPricked = false;

    // The current project data
    private final EditorProject mEditorProject;
    private final EditorConfig mEditorConfig;
    private final SceneScript mSceneScript;

    //
    public OLDSceneScriptEditor(final EditorProject project) {
        // Initialize the editor project
        mEditorProject = project;
        // Initialize the scene script
        mSceneScript = mEditorProject.getSceneScript();
        // Initialize the editor config
        mEditorConfig = mEditorProject.getEditorConfig();

        // Initialize The Status Label
        mStatusLabel = new CaretStatusLabel("");

        // Initialize The Editor Pane
        mEditorPane = new ScriptEditorPane(mEditorProject);
        mEditorPane.addCaretListener(mStatusLabel);
        mEditorPane.getDocument().addDocumentListener(this);

        // Initialize The Scroll Pane
        mScrollPane = new JScrollPane(mEditorPane);
        mScrollPane.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));

        // Initialize The Function Definition Panel
        mFunctionEditor = new FunctionsEditor(mEditorProject);

        mTabPane = new JTabbedPane();
        mTabPane.setUI(new BasicTabbedPaneUI());
        mTabPane.setOpaque(false);

        // Initialize The Scroll Pane
        mElementPane = new SceneElementDisplay(mEditorProject);

        mGesticonButton = new JButton(Boolean.valueOf(mEditorConfig.getProperty("showsceneelements"))
                ? Preferences.ICON_MORE_STANDARD
                : Preferences.ICON_LESS_STANDARD);
        mGesticonButton.setRolloverIcon(Boolean.valueOf(mEditorConfig.getProperty("showsceneelements"))
                ? Preferences.ICON_MORE_ROLLOVER
                : Preferences.ICON_LESS_ROLLOVER);
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

        if (Boolean.valueOf(mEditorConfig.getProperty("showsceneelements"))) {
            scriptSplitPane.setDividerLocation(250);
        }
        if (!Boolean.valueOf(mEditorConfig.getProperty("showsceneelements"))) {
            scriptSplitPane.setDividerLocation(0);
        }
        Box bxBottom = Box.createHorizontalBox();
        bxBottom.add(scriptSplitPane);
        //Script Panel

        mScriptTabPanel.setLayout(new BoxLayout(mScriptTabPanel, BoxLayout.Y_AXIS));
        mScriptTabPanel.add(bxTop);
        mScriptTabPanel.add(bxBottom);

        addTab("Scenes        ", mScriptTabPanel);
        addTab("Functions     ", mFunctionEditor);
//        addTab("DialogAct [Experimental]", mDialogActEditor);

        // Initialize The Components
        setLayout(new OverlayLayout(this));
        setBorder(BorderFactory.createEmptyBorder());
        //add(mTabPane, BorderLayout.CENTER);

        mPinButton = new JButton();
        pinPricked = mEditorConfig.sAUTOHIDE_BOTTOMPANEL;
        setPin(pinPricked);
        mPinButton.setContentAreaFilled(false);
        //mPinButton.setMargin(new Insets(0, 10, 20, 10));
        mPinButton.setFocusable(false);
        mPinButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setPin(!pinPricked);
            }
        });
        sanitizeTinyButton(mPinButton);
        Box VpinBox = Box.createVerticalBox();
        Box HpinBox = Box.createHorizontalBox();
        HpinBox.add(Box.createHorizontalGlue());
        HpinBox.add(mPinButton);
        VpinBox.add(HpinBox);
        VpinBox.add(Box.createVerticalGlue());
        //add(Box.createHorizontalGlue());
        add(VpinBox, BorderLayout.AFTER_LINE_ENDS);
        add(mTabPane, BorderLayout.CENTER);
        add(mStatusLabel, BorderLayout.SOUTH);

        // Register As Event Listener
        mEventDispatcher.register(this);

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

    // Set the pin pricked flag
    public final void setPin(boolean state) {
        pinPricked = state;
        mPinButton.setIcon(pinPricked ? ICON_PIN_ROLLOVER : ICON_PIN_STANDARD);
        mPinButton.setRolloverIcon(pinPricked ? ICON_PIN_STANDARD : ICON_PIN_ROLLOVER);
        mEditorConfig.setProperty("autohidebottombar", String.valueOf(pinPricked));
    }

    private void sanitizeTinyButton(JButton b) {
        Dimension bDim = new Dimension(30, 30);

        b.setMinimumSize(bDim);
        b.setMaximumSize(bDim);
        b.setPreferredSize(bDim);
        //b.setOpaque(false);

//      b.setContentAreaFilled(false);
//      b.setFocusable(false);
        b.setBorder(BorderFactory.createEmptyBorder());
    }

    //Adds a tab to the tabbedpane with a plus icon
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
                    mAddButton.setIcon(Preferences.ICON_PLUS_ROLLOVER);
                }
            }

            @Override
            public void mouseExited(MouseEvent me) {
                mAddButton.setIcon(Preferences.ICON_PLUS_STANDARD);
            }

            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                if (mTabPane.getSelectedIndex() == mAddButton.getTabPos()) {
                    if (content instanceof FunctionsEditor) {
                        ((FunctionsEditor) content).addNewFunction();
                    } //                    else if (content instanceof DialogActEditor) {
                    //                        //PLUS ACTION FOR DIALGOACTEDITOR
                    //                    }
                    else {
                        mEditorPane.append("scene_@@ SceneName:\n" + "character: Text.\n\n");
                        mEditorPane.requestFocusInWindow();
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
            //mTabPane.setSelectedIndex(mTabPane.getTabCount() - 1);
        }

        tabCounter++;
    }

    // Get the pin pricked flag
    public boolean isPinPricked() {
        return pinPricked;
    }
//
//    // Set the pin pricked flag

    public void setPinPricked() {
        setPin(true); // true pricks the pin
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
                mTabPane.setSelectedComponent(mScriptTabPanel);
            } else if (((TreeEntrySelectedEvent) event).getmEntry().getText().contains("Functions")) {
                mTabPane.setSelectedComponent(mFunctionEditor);
            }
//            else if (((TreeEntrySelectedEvent) event).getmEntry().getText().contains("Dialog")) {
//                mTabPane.setSelectedComponent(mDialogActEditor);
//            }
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
            EditorInstance.getInstance().refresh();
            // Print some information
            mLogger.message("Updating editor after successful parsing the scenescript document");
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

        if (Boolean.valueOf(mEditorConfig.getProperty("showsceneelements"))) {
            mGesticonButton.setIcon(Preferences.ICON_LESS_STANDARD);
            mEditorConfig.setProperty("showsceneelements", "false");
            //mPreferences.save(getPreferencesFileName());
            scriptSplitPane.setDividerLocation(0);
        } else {
            mGesticonButton.setIcon(Preferences.ICON_MORE_STANDARD);
            mEditorConfig.setProperty("showsceneelements", "true");
            //mPreferences.save(getPreferencesFileName());
            scriptSplitPane.setDividerLocation(250);
        }
        mGesticonButton.setRolloverIcon(Boolean.valueOf(mEditorConfig.getProperty("showsceneelements"))
                ? Preferences.ICON_MORE_ROLLOVER
                : Preferences.ICON_LESS_ROLLOVER);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void close() {

        // Remove As Event Listener
        mEventDispatcher.remove(this);

        // Remove All Observers
//        mObservable.deleteObservers();
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
//    @Override
//    public void update(final java.util.Observable obs, final Object obj) {
//        mObservable.notify(obj);
//    }
    // Refresh the visual appearance
    public final void refresh() {
        mElementPane.refresh();
        mEditorPane.refresh();
        // TODO: Refresh the appearance
    }
}
