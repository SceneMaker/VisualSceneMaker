package de.dfki.vsm.editor.project.auxiliary.LLM;

import de.dfki.vsm.PreferencesDesktop;
import de.dfki.vsm.model.LLM.LLMController;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

//this class creates the frontend of the LLM panel for its registry
//it has a run button and an input field for text
public class LLMPanel extends JPanel {
    // The system logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    //private final JPanel mLLMPanel = new JPanel();
    private JButton mRunLLMButton;
    private final JScrollPane mLLMScrollPane;

    //TODO WOULD BE BETTER TO INSTANCIATION IN BACKEND AND PASS REFERENCE SOMEHOW
    private LLMController llmController = new LLMController();

    private final JEditorPane mLLMEditorPane;
    public LLMPanel() {
        //LLM Panel TODO ADD PANEL STUFF HERE
        //EDITOR
        // Initialize The Editor Pane
        mLLMEditorPane = new JEditorPane();
        //mLLMEditorPane.addCaretListener(mStatusLabel);
        //mLLMEditorPane.getDocument().addDocumentListener(this);

        // Initialize The Scroll Pane
        mLLMScrollPane = new JScrollPane(mLLMEditorPane);
        mLLMScrollPane.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));

        //apply button
        mRunLLMButton = new JButton(PreferencesDesktop.ICON_LESS_STANDARD);//TODO CHANGE TO PLAY BUTTON
        mRunLLMButton.setRolloverIcon(PreferencesDesktop.ICON_LESS_ROLLOVER);

        //here is the backande called when the play button is pressed
        mRunLLMButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent me) {
               llmController.execute(mLLMEditorPane.getText());
            }
        });
        mRunLLMButton.setContentAreaFilled(false);
        mRunLLMButton.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        mRunLLMButton.setFocusable(false);
        //
        Box bxTopLLM = Box.createHorizontalBox();
        bxTopLLM.add(mRunLLMButton);

        this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        this.add(bxTopLLM);
        this.add(mLLMScrollPane);
    }
}
