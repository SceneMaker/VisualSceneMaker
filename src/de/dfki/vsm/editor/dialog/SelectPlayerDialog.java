package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.runtime.project.RunTimeProject;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

/**
 * Created by alvaro on 2/6/16.
 */
public class SelectPlayerDialog extends JDialog
{
    
    private JLabel mPlayerNameLabel;
    private JLabel mAgentNameLabel;
    private JLabel mProjectNameLabel;
    private JLabel mFeatureHostLabel;
    private JLabel mFeaturePortLabel;
    private JLabel mCharacterNameLabel;
    
    private JTextField mPlayerNameText;
    private JTextField mAgentNameText;
    private JTextField mProjectNameText;
    private HintTextField mFeatureHostText;
    private HintTextField mFeaturePortText;
    private HintTextField mCharacterNameText;
    private Dimension labelSize = new Dimension(100, 30);
    private Dimension textFielSize = new Dimension(250, 30);
    private final RunTimeProject mProject;
    
    private String mPlayerName;
    private String mAgentName;
    private String mCharacterName;
    private ArrayList<String> mMissingAgents;
    private int currentIndex = 0;

//    private JButton mNextButton;
//    private JButton mPreviousButton;
    private OKButton mOkButton;
    private CancelButton mCancelButton;
    private boolean finished = false;
    
    public SelectPlayerDialog(JDialog parent, String title, boolean modal)
    {
        super(EditorInstance.getInstance(), "Create new  Agent", true);
        // super(parent, title, modal);
        mProject = null;
        initComponents();
    }
    
    public SelectPlayerDialog(RunTimeProject project, ArrayList<String> missingAgents, String playerName)
    {
        super(EditorInstance.getInstance(), "Create  a new agent", false);
        mAgentName = "Test";
        mMissingAgents = missingAgents;
        mCharacterName = missingAgents.get(0);
        mProject = project;
        mPlayerName = playerName;
        //CHANGE MODALITY
        setModalityType(Dialog.ModalityType.DOCUMENT_MODAL);
        initComponents();
    }

    /* Set the correct size of the components
    *
            * @param jb
    * @param dim
     */
    private void sanitizeComponent(JComponent jb, Dimension dim)
    {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
    }
    
    public void initComponents()
    {
        mPlayerNameLabel = new JLabel("Player Name");
        mAgentNameLabel = new JLabel("Agent Name");
        mProjectNameLabel = new JLabel("Project Name");
        mFeatureHostLabel = new JLabel("Host");
        mFeaturePortLabel = new JLabel("Port");
        mCharacterNameLabel = new JLabel("Character Name");
        
        mPlayerNameText = new JTextField();
        mAgentNameText = new JTextField();
        mProjectNameText = new JTextField();
        mFeatureHostText = new HintTextField("localhost");
        mFeaturePortText = new HintTextField("7777");
        mCharacterNameText = new HintTextField(mCharacterName);
        
        mCharacterNameText.setText(mCharacterName);

        mOkButton = new OKButton();
        mOkButton.setText((mMissingAgents.size() > 1) ? "Next" : "Finish");
        mOkButton.setIcon((mMissingAgents.size() > 1) ? Preferences.ICON_NEXT_STANDARD : Preferences.ICON_OK_STANDARD);
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent evt)
            {
                nextActionPerformed();
            }
            public void mouseEntered(MouseEvent me) {
                setBackground(new Color(82, 127, 255));
                if (isEnabled() && currentIndex < (mMissingAgents.size() -1 ))
                {
                    mOkButton.setIcon(Preferences.ICON_NEXT_ROLLOVER);   
                }
                else
                {
                    mOkButton.setIcon(Preferences.ICON_OK_ROLLOVER); 
                }
            }
            public void mouseExited(MouseEvent me) {
                setBackground(new Color(255, 255, 255));
                if (isEnabled() && currentIndex < (mMissingAgents.size() -1 ))
                {
                    mOkButton.setIcon(Preferences.ICON_NEXT_STANDARD);   
                }
                else
                {
                    mOkButton.setIcon(Preferences.ICON_OK_STANDARD); 
                }
            }
        });
        mCancelButton = new CancelButton();
//        mCancelButton.setEnabled(false);
        mCancelButton.setBounds(100, 35, 90, 20);
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent evt)
            {
                previousActionPerformed();
            }
            public void mouseEntered(MouseEvent me) {
                setBackground(new Color(82, 127, 255));
                if (currentIndex == 0)
                {
                    mCancelButton.setIcon(Preferences.ICON_CANCEL_ROLLOVER);   
                }
                else
                {
                    mCancelButton.setIcon(Preferences.ICON_PREVIOUS_ROLLOVER); 
                }
            }
            public void mouseExited(MouseEvent me) {
                setBackground(new Color(255, 255, 255));
                if ( currentIndex > 0)
                {
                    mCancelButton.setIcon(Preferences.ICON_PREVIOUS_STANDARD);   
                }
                else
                {
                    mCancelButton.setIcon(Preferences.ICON_CANCEL_STANDARD); 
                }
            }
        });
        
        mProjectNameText.setText(mProject.getProjectName());
        mPlayerNameText.setText(mPlayerName);
        
        JPanel mButtonPanel = new JPanel();
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createHorizontalStrut(10));
        
        JPanel mNavigationPanel = new JPanel();
        //mNavigationPanel.setBackground(new Color(255,255,255));
        mNavigationPanel.setLayout(new BoxLayout(mNavigationPanel, BoxLayout.LINE_AXIS));
        //mNavigationPanel.add(Box.createHorizontalGlue());
//        mNavigationPanel.add(mPreviousButton);
        mNavigationPanel.add(Box.createHorizontalGlue());
//        mNavigationPanel.add(mNextButton);
        //mNavigationPanel.add(Box.createHorizontalStrut(10));

        mPlayerNameText.setEditable(false);
        Box nameBox = Box.createHorizontalBox();
        sanitizeComponent(mPlayerNameLabel, labelSize);
        sanitizeComponent(mPlayerNameText, textFielSize);
        
        nameBox.add(mPlayerNameLabel);
        nameBox.add(Box.createHorizontalStrut(10));
        nameBox.add(mPlayerNameText);
        
        Box agentBox = Box.createHorizontalBox();
        
        sanitizeComponent(mAgentNameLabel, labelSize);
        sanitizeComponent(mAgentNameText, textFielSize);
        
        agentBox.add(mAgentNameLabel);
        agentBox.add(Box.createHorizontalStrut(10));
        agentBox.add(mAgentNameText);
        
        Box projectBox = Box.createHorizontalBox();
        
        sanitizeComponent(mProjectNameLabel, labelSize);
        sanitizeComponent(mProjectNameText, textFielSize);
        
        projectBox.add(mProjectNameLabel);
        projectBox.add(Box.createHorizontalStrut(10));
        projectBox.add(mProjectNameText);
        mProjectNameText.setEditable(false);
        
        Box hostBox = Box.createHorizontalBox();
        
        sanitizeComponent(mFeatureHostLabel, labelSize);
        sanitizeComponent(mFeatureHostText, textFielSize);
        
        hostBox.add(mFeatureHostLabel);
        hostBox.add(Box.createHorizontalStrut(10));
        hostBox.add(mFeatureHostText);
        
        Box portBox = Box.createHorizontalBox();
        
        sanitizeComponent(mFeaturePortLabel, labelSize);
        sanitizeComponent(mFeaturePortText, textFielSize);
        
        portBox.add(mFeaturePortLabel);
        portBox.add(Box.createHorizontalStrut(10));
        portBox.add(mFeaturePortText);
        
        Box charBox = Box.createHorizontalBox();
        
        sanitizeComponent(mCharacterNameLabel, labelSize);
        sanitizeComponent(mCharacterNameText, textFielSize);
        
        charBox.add(mCharacterNameLabel);
        charBox.add(Box.createHorizontalStrut(10));
        charBox.add(mCharacterNameText);
        
        Box finalBox = Box.createVerticalBox();
        
        finalBox.add(mNavigationPanel);
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.add(projectBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(nameBox);
        
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(charBox);
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.add(mButtonPanel);
        finalBox.add(Box.createVerticalStrut(15));
        
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.setBorder(new EmptyBorder(10, 20, 10, 10));
        getContentPane().add(finalBox, BorderLayout.WEST);
        //addComponent(finalBox, 10, 20, 400, 250);
        //packComponents(420, 270);
        setSize(410, 330);
        Dimension bounds = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension abounds = getSize();
        setLocation((bounds.width - abounds.width) / 2, (bounds.height - abounds.height) / 3);
        setResizable(false);
        setVisible(true);
        mOkButton.requestFocus();
        
    }
    
    public void showDialog()
    {
        //mProjectNameText.setText(mProjectName);
        mAgentNameText.setText(mAgentName);
        
        this.setVisible(true);
    }
    
    private void nextActionPerformed()
    {
        
        if (currentIndex < mMissingAgents.size() - 1)
        {
            String name = mMissingAgents.get(currentIndex);
            if (!name.equals(mCharacterNameText.getText()))
            {
                mMissingAgents.remove(currentIndex);
                mMissingAgents.add(currentIndex, mCharacterNameText.getText());
            }
            currentIndex++;
            if (currentIndex == mMissingAgents.size() - 1)
            {
//                mNextButton.setEnabled(false);
                mOkButton.setIcon(Preferences.ICON_OK_STANDARD);
                mOkButton.setText("Finish");
            }
            
            if (currentIndex < mMissingAgents.size())
            {
                mCharacterNameText.setText(mMissingAgents.get(currentIndex));
                mCancelButton.setIcon(Preferences.ICON_PREVIOUS_STANDARD);
                mCancelButton.setText("Back");
            }
        }
        else
        {
            okActionPerformed();
        }
    }
    
    private void previousActionPerformed()
    {
        if (currentIndex > 0)
        {
            
            currentIndex--;
            if (currentIndex == 0)
            {
                mCancelButton.setText("Cancel");
                mCancelButton.setIcon(Preferences.ICON_CANCEL_STANDARD);
//                mCancelButton.setEnabled(false);
            }
            mCharacterNameText.setText(mMissingAgents.get(currentIndex));
            
            if (mMissingAgents.size() > 1)
            {
                mOkButton.setIcon(Preferences.ICON_NEXT_STANDARD); 
                mOkButton.setText("Next");
            }
        }
        else
        {
            cancelActionPerformed();            
        }
    }
    
    protected void okActionPerformed()
    {
        for (String agent : mMissingAgents)
        {
            parseAgent(agent);
        }
        finished = true;
        dispose();
        //dispose(mOkButton);
    }
    
    private void parseAgent(String agent)
    {
        String newXMLAgent = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                + "<Project name=\"" + mProjectNameText.getText() + "\">"
                + "<Agents>"
                + "    <Agent name=\"" + agent + "\" player=\"" + mPlayerNameText.getText() + "\">"
                + "      <Feature key=\"host\" value=\"localhost\"/>"
                + "      <Feature key=\"port\" value=\"7777\"/>"
                + "    </Agent>"
                + " </Agents>"
                + "</Project>";
        mProject.parseProjectConfigFromString(newXMLAgent);
    }
    
    protected void cancelActionPerformed()
    {
        finished = false;
        dispose();
        //dispose(Button.CANCEL);
    }
    
    public boolean isFinished()
    {
        return finished;
    }
}
