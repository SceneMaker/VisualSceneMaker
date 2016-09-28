package de.dfki.vsm.xtension.wizard;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.BoxLayout;
import static javax.swing.BoxLayout.X_AXIS;
import static javax.swing.BoxLayout.Y_AXIS;

/**
 * @author Gregor Mehlmann
 */
public final class WizardExecutor extends ActivityExecutor {

    // Wizard panels
    private JFrame mMainWizardFrame;
    private JPanel mMainWizardPanel;
    private JPanel mInputWizardPanel;
    private JPanel mUserWizardPanel;
    private JPanel mAgentWizardPanel;
    private JPanel mUserStatusPanel;
    private JPanel mUserSignalPanel;
    private JPanel mUserInputPanel;
    private JPanel mAgentStatusPanel;
    private JPanel mAgentSignalPanel;
    private JPanel mAgentInputPanel;
    private JPanel mUserVerbalPanel;
    private JPanel mUserNonverbPanel;
    private JPanel mOutputWizardPanel;
    // Output console
    private JTextPane mOutputWizardArea;
    private JScrollPane mOutputWizardPane;
    // Status labels
    private JLabel mUserStateLabel;
    private JLabel mAgentStateLabel;
    private JLabel mUserRoleLabel;
    private JLabel mAgentRoleLabel;
    private JLabel mUserActLabel;
    private JLabel mAgentActLabel;
    // Status Buttons
    private JButton mUserStateButton;
    private JButton mAgentStateButton;
    // Signal buttons
    private JButton mUserRequestButton;
    private JButton mUserAcceptButton;
    private JButton mUserRejectButton;
    private JButton mUserOfferButton;
    private JButton mUserLeaveButton;
    private JButton mUserClaimButton;
    private JButton mAgentRequestButton;
    private JButton mAgentAcceptButton;
    private JButton mAgentRejectButton;
    private JButton mAgentOfferButton;
    private JButton mAgentLeaveButton;
    private JButton mAgentClaimButton;
    //
    private JLabel mUserElicitLabel;
    private JButton mUserElicitButton;
    // Input panels
    private JLabel mUserVoiceLabel;
    private JButton mUserVoiceButton;
    // Affect Slider
    private JLabel mAffectLabel;
    private JSlider mAffectSlider;
    // Expressions
    private JLabel mExpressionLabel;
    private JComboBox mExpressionCombo;
    private DefaultComboBoxModel mEmotionModel;
    // Eyegaze
    private JLabel mEyegazeLabel;
    private JComboBox mEyegazeCombo;
    private DefaultComboBoxModel mObjectModel;
    // Scenescript
    private JLabel mScriptLabel;
    private JComboBox mScriptCombo;
    private DefaultComboBoxModel mScriptModel;
    // Style attributes
    private SimpleAttributeSet mDateStyle;
    private SimpleAttributeSet mTextStyle;
    // The scene player 
    //private final AOCScenePlayer mPlayer;
    //private final RunTimeProject mProject;
    //private final RunTimeInstance mRunTime;
    private final SceneScript mSceneScript;
    // The date format
    private final SimpleDateFormat mFormat
            = new SimpleDateFormat("HH:mm:ss.SSS");
    //
    private WizardTimer mTimer;

    // Launch the executor 
    @Override
    public void launch() {
        show();
        //
        mTimer = new WizardTimer(10);
        mTimer.start();

    }

    // Unload the executor 
    @Override
    public void unload() {
        hide();
        //
        mTimer.abort();
        try {
            mTimer.join();
        } catch (final InterruptedException exc) {
            mLogger.failure(exc.toString());
        }
    }

    @Override
    public final String marker(final long id) {
        // Loquendo style bookmarks
        return null;//"\\book=" + id + "";
    }

    @Override
    public final void execute(final AbstractActivity activity) {
        // Get the current worker
        final ActivityWorker worker = (ActivityWorker) Thread.currentThread();
        // Get activity information
        final String name = activity.getName();
        final String mode = activity.getMode();
        final String actor = activity.getActor();
        final String type = activity.getType().name();
        final String text = activity.getText();
        final LinkedList<ActionFeature> features = activity.getFeatures();

        mLogger.message(actor + ": " + name);

        if (activity instanceof ActionActivity) {
            final ActionActivity action = (ActionActivity) activity;
            if (name.equals("refresh")) {
                mLogger.message("Refreshing Wizard");
                refresh();
            }
        }
    }

    // Create a new agent wizard
    public WizardExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        // Initialize player reference 
        //mPlayer = mProject;
        //mRunTime = player.getRunTime();
        //mProject = mPlayer.getProject();
        mSceneScript = mProject.getSceneScript();
        // Load the data and init GUI
        load();
        init();
    }

    // Load the data for the models
    private void load() {
        loadSceneGroups();
        loadEmotionExps();
        loadObjectNames();
    }

    // Initialize the expression box
    private void loadObjectNames() {
        // Create the emotion model
        mObjectModel = new DefaultComboBoxModel();
        // Add the emotion expressions
        mObjectModel.addElement("pen");
        mObjectModel.addElement("cup");
        mObjectModel.addElement("book");
        mObjectModel.addElement("robot");
        // Deselect everything first
        mObjectModel.setSelectedItem(null);
    }

    // Initialize the expression box
    private void loadEmotionExps() {
        // Create the emotion model
        mEmotionModel = new DefaultComboBoxModel();
        // Add the emotion expressions
        mEmotionModel.addElement("neutral");
        mEmotionModel.addElement("joy");
        mEmotionModel.addElement("sad");
        mEmotionModel.addElement("fear");
        mEmotionModel.addElement("anger");
        mEmotionModel.addElement("disgust");
        mEmotionModel.addElement("surprise");
        // Deselect everything first
        mEmotionModel.setSelectedItem(null);
    }

    // Initialize the scene group box
    private void loadSceneGroups() {
        // Create the script model
        mScriptModel = new DefaultComboBoxModel();
        // Fill the scene group box
        for (final SceneGroup group
                : mSceneScript.getOrderedGroupSet().descendingSet()) {
            mScriptModel.addElement(group.getName());
        }
        // Deselect everything first
        mScriptModel.setSelectedItem(null);
    }

    // Initialize the agent wizard
    private void init() {
        initAttributes();
        initComponents();
    }

    // Initialize the attributes
    private void initAttributes() {
        mDateStyle = new SimpleAttributeSet();
        mDateStyle.addAttribute(StyleConstants.Foreground, Color.BLUE);
        // Initialize the text attributes
        mTextStyle = new SimpleAttributeSet();
        mTextStyle.addAttribute(StyleConstants.Italic, Boolean.TRUE);
        mTextStyle.addAttribute(StyleConstants.Foreground, Color.DARK_GRAY);
    }

    // Initialize the components
    private void initComponents() {
        // Create the user status panel ////////////////////////////////////////
        mUserStateButton = new JButton();
        mUserStateButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //final String userState = (String) mRunTime.getValueOf(mProject, "UserState").getValue();
                //if (userState.equals("present")) {
                //    mPlayer.event("state", "user", "leave");
                //} else {
                //    mPlayer.event("state", "user", "enter");
                //}
            }
        });
        mUserActLabel = new JLabel("Action:");
        mUserRoleLabel = new JLabel("Role:");
        mUserStateLabel = new JLabel("Status:");
        mUserStatusPanel = new JPanel();
        mUserStatusPanel.setLayout(new GridLayout(2, 2));
        mUserStatusPanel.setBorder(BorderFactory.createTitledBorder("Status Variables"));
        mUserStatusPanel.add(mUserStateLabel);
        mUserStatusPanel.add(mUserStateButton);
        mUserStatusPanel.add(mUserRoleLabel);
        mUserStatusPanel.add(mUserActLabel);

        // Create the user signal panel ////////////////////////////////////////
        mUserRequestButton = new JButton("Request");
        mUserRequestButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "user", "request");
            }
        });
        mUserAcceptButton = new JButton("Accept");
        mUserAcceptButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "user", "accept");
            }
        });
        mUserRejectButton = new JButton("Reject");
        mUserRejectButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "user", "reject");
            }
        });
        mUserOfferButton = new JButton("Offer");
        mUserOfferButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "user", "offer");
            }
        });
        mUserLeaveButton = new JButton("Leave");
        mUserLeaveButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "user", "leave");
            }
        });
        mUserClaimButton = new JButton("Claim");
        mUserClaimButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "user", "claim");
            }
        });
        mUserSignalPanel = new JPanel();
        mUserSignalPanel.setLayout(new GridLayout(2, 3));
        mUserSignalPanel.setBorder(BorderFactory.createTitledBorder("Turn Regulation"));
        mUserSignalPanel.add(mUserRequestButton);
        mUserSignalPanel.add(mUserAcceptButton);
        mUserSignalPanel.add(mUserRejectButton);
        mUserSignalPanel.add(mUserOfferButton);
        mUserSignalPanel.add(mUserLeaveButton);
        mUserSignalPanel.add(mUserClaimButton);

        // Create the user verbal panel ////////////////////////////////////////
        mUserVoiceLabel = new JLabel("Utterance:");
        mUserVoiceLabel.setBounds(5, 30, 100, 25);
        mUserVoiceLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserVoiceButton = new JButton();
        mUserVoiceButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //final String userVoice = (String) mRunTime.getValueOf(mProject, "UserVoice").getValue();
                //if (userVoice.equals("speaking")) {
                //    mPlayer.event("voice", "user", "stop");
                //} else {
                //    mPlayer.event("voice", "user", "start");
                //}
            }
        });
        mUserVerbalPanel = new JPanel();
        mUserVerbalPanel.setLayout(new GridLayout());
        mUserVerbalPanel.setBorder(BorderFactory.createEmptyBorder());
        mUserVerbalPanel.add(mUserVoiceLabel);
        mUserVerbalPanel.add(mUserVoiceButton);

        // Create the user nonverbal panel /////////////////////////////////////
        mExpressionLabel = new JLabel("Expression:");
        mExpressionLabel.setBounds(240, 40, 100, 25);
        mExpressionLabel.setBorder(BorderFactory.createEmptyBorder());
        mExpressionCombo = new JComboBox(mEmotionModel);
        mExpressionCombo.setBackground(Color.WHITE);
        mExpressionCombo.setBounds(250, 65, 100, 25);
        mExpressionCombo.setBorder(BorderFactory.createEtchedBorder());
        mExpressionCombo.addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(final ItemEvent event) {
                //if (event.getStateChange() == ItemEvent.SELECTED) {
                //    mPlayer.event("facs", "user", ((String) event.getItem()));
                //}
            }
        });
        mEyegazeLabel = new JLabel("Eyegaze:");
        mEyegazeLabel.setBorder(BorderFactory.createEmptyBorder());
        mEyegazeCombo = new JComboBox(mObjectModel);
        mEyegazeCombo.setBackground(Color.WHITE);
        mEyegazeCombo.setBorder(BorderFactory.createEtchedBorder());
        mEyegazeCombo.addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(final ItemEvent event) {
                //if (event.getStateChange() == ItemEvent.SELECTED) {
                //    mPlayer.event("gaze", "user", ((String) event.getItem()));
                //}
            }
        });

        mUserElicitLabel = new JLabel("Feedback:");
        mUserElicitLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserElicitButton = new JButton("Elicit:");
        mUserElicitButton.setBackground(Color.WHITE);
        mUserElicitButton.setBorder(BorderFactory.createEmptyBorder());
        mUserElicitButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                // mPlayer.event("feedback", "user", "elicit");
            }
        });

        mAffectLabel = new JLabel("Activity:");
        mAffectLabel.setBorder(BorderFactory.createEmptyBorder());
        mAffectSlider = new JSlider(JSlider.HORIZONTAL);
        mAffectSlider.setBackground(Color.WHITE);
        mAffectSlider.setBorder(BorderFactory.createEmptyBorder());
        mAffectSlider.addChangeListener(new ChangeListener() {

            @Override
            public void stateChanged(final ChangeEvent event) {
                //if (!mAffectSlider.getValueIsAdjusting()) {
                //    mPlayer.event("mood", "user", String.valueOf(mAffectSlider.getValue()));
                //}
            }
        });
        mUserNonverbPanel = new JPanel();
        mUserNonverbPanel.setLayout(new GridLayout(4, 3));
        mUserNonverbPanel.setBorder(BorderFactory.createEmptyBorder());
        mUserNonverbPanel.add(mExpressionLabel);
        mUserNonverbPanel.add(mExpressionCombo);
        mUserNonverbPanel.add(mEyegazeLabel);
        mUserNonverbPanel.add(mEyegazeCombo);
        mUserNonverbPanel.add(mUserElicitLabel);
        mUserNonverbPanel.add(mUserElicitButton);
        mUserNonverbPanel.add(mAffectLabel);
        mUserNonverbPanel.add(mAffectSlider);

        // Create the user input panel ///////////////////////////////////////       
        mUserInputPanel = new JPanel();
        mUserInputPanel.setLayout(new BoxLayout(mUserInputPanel, Y_AXIS));
        mUserInputPanel.setBorder(BorderFactory.createTitledBorder("Input Simulation"));
        mUserInputPanel.add(mUserNonverbPanel);
        mUserInputPanel.add(mUserVerbalPanel);

        // Create the agent status panel ///////////////////////////////////////
        mAgentStateButton = new JButton();
        mAgentStateButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //final String agentState = (String) mRunTime.getValueOf(mProject, "AgentState").getValue();
                // TODO: get variable values from interpreter
                //if (agentState.equals("present")) {
                //    mPlayer.event("state", "agent", "leave");
                //} else {
                //    mPlayer.event("state", "agent", "enter");
                //}
            }
        });
        mAgentActLabel = new JLabel("Action:");
        mAgentRoleLabel = new JLabel("Role:");
        mAgentStateLabel = new JLabel("Status:");
        mAgentStatusPanel = new JPanel();
        mAgentStatusPanel.setLayout(new GridLayout(2, 2));
        mAgentStatusPanel.setBorder(BorderFactory.createTitledBorder("Status Variables"));
        mAgentStatusPanel.add(mAgentStateLabel);
        mAgentStatusPanel.add(mAgentStateButton);
        mAgentStatusPanel.add(mAgentRoleLabel);
        mAgentStatusPanel.add(mAgentActLabel);

        // Create the agent signal panel ///////////////////////////////////////
        mAgentRequestButton = new JButton("Request");
        mAgentRequestButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "agent", "request");
            }
        });
        mAgentAcceptButton = new JButton("Accept");
        mAgentAcceptButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "agent", "accept");
            }
        });
        mAgentRejectButton = new JButton("Reject");
        mAgentRejectButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                // mPlayer.event("turn", "agent", "reject");
            }
        });
        mAgentOfferButton = new JButton("Offer");
        mAgentOfferButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "agent", "offer");
            }
        });
        mAgentLeaveButton = new JButton("Leave");
        mAgentLeaveButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "agent", "leave");
            }
        });
        mAgentClaimButton = new JButton("Claim");
        mAgentClaimButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                //mPlayer.event("turn", "agent", "claim");
            }
        });
        mAgentSignalPanel = new JPanel();
        mAgentSignalPanel.setLayout(new GridLayout(2, 3));
        mAgentSignalPanel.setBorder(BorderFactory.createTitledBorder("Turn Regulation"));
        mAgentSignalPanel.add(mAgentRequestButton);
        mAgentSignalPanel.add(mAgentAcceptButton);
        mAgentSignalPanel.add(mAgentRejectButton);
        mAgentSignalPanel.add(mAgentOfferButton);
        mAgentSignalPanel.add(mAgentLeaveButton);
        mAgentSignalPanel.add(mAgentClaimButton);

        // Create the agent input panel ///////////////////////////////////////
        mScriptLabel = new JLabel("Play Scene:");
        mScriptLabel.setBorder(BorderFactory.createEmptyBorder());
        mScriptCombo = new JComboBox(mScriptModel);
        mScriptCombo.setBackground(Color.WHITE);
        mScriptCombo.setBorder(BorderFactory.createEtchedBorder());
        mScriptCombo.addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(final ItemEvent event) {
                if (event.getStateChange() == ItemEvent.SELECTED) {
                    //mPlayer.event("contrib", "agent", (String) event.getItem());
                }

            }
        });
        mAgentInputPanel = new JPanel();
        mAgentInputPanel.setLayout(new GridLayout(3, 2));
        mAgentInputPanel.setBorder(BorderFactory.createTitledBorder("Input Simulation"));
        mAgentInputPanel.add(mScriptLabel);
        mAgentInputPanel.add(mScriptCombo);
        mAgentInputPanel.add(new JPanel());
        mAgentInputPanel.add(new JPanel());
        mAgentInputPanel.add(new JPanel());
        mAgentInputPanel.add(new JPanel());

        // Create the user wizard panel ////////////////////////////////////////
        mUserWizardPanel = new JPanel();
        mUserWizardPanel.setLayout(new BoxLayout(mUserWizardPanel, Y_AXIS));
        mUserWizardPanel.setBorder(BorderFactory.createTitledBorder("User Wizard"));
        mUserWizardPanel.add(mUserStatusPanel);
        mUserWizardPanel.add(mUserSignalPanel);
        mUserWizardPanel.add(mUserInputPanel);

        // Create the agent wizard panel ////////////////////////////////////////
        mAgentWizardPanel = new JPanel();
        mAgentWizardPanel.setLayout(new BoxLayout(mAgentWizardPanel, Y_AXIS));
        mAgentWizardPanel.setBorder(BorderFactory.createTitledBorder("Agent Wizard"));
        mAgentWizardPanel.add(mAgentStatusPanel);
        mAgentWizardPanel.add(mAgentSignalPanel);
        mAgentWizardPanel.add(mAgentInputPanel);

        // Create the input wizard panel ///////////////////////////////////////
        mInputWizardPanel = new JPanel();
        mInputWizardPanel.setLayout(new BoxLayout(mInputWizardPanel, X_AXIS));
        mInputWizardPanel.setBorder(BorderFactory.createEmptyBorder());
        mInputWizardPanel.add(mUserWizardPanel);
        mInputWizardPanel.add(mAgentWizardPanel);

        // Create the output console pane //////////////////////////////////////
        mOutputWizardArea = new JTextPane(new DefaultStyledDocument());
        mOutputWizardArea.setBorder(BorderFactory.createEmptyBorder());
        mOutputWizardArea.setEditable(false);
        mOutputWizardPane = new JScrollPane(mOutputWizardArea);
        mOutputWizardPane.setBorder(BorderFactory.createEtchedBorder());
        mOutputWizardPanel = new JPanel();
        mOutputWizardPanel.setLayout(new BoxLayout(mOutputWizardPanel, X_AXIS));
        mOutputWizardPanel.setBorder(BorderFactory.createEmptyBorder());
        mOutputWizardPanel.setPreferredSize(new Dimension(0, 150));
        mOutputWizardPanel.add(mOutputWizardPane);

        // Create the main wizard panel ////////////////////////////////////////
        mMainWizardPanel = new JPanel();
        mMainWizardPanel.setLayout(new BoxLayout(mMainWizardPanel, Y_AXIS));
        mMainWizardPanel.setBorder(BorderFactory.createEmptyBorder());
        mMainWizardPanel.add(mInputWizardPanel);
        mMainWizardPanel.add(mOutputWizardPanel);

        // Create the main wizard frame ////////////////////////////////////////
        mMainWizardFrame = new JFrame("Behavior Simulation Wizard");
        mMainWizardFrame.setContentPane(mMainWizardPanel);
        mMainWizardFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        mMainWizardFrame.pack();
        // Reset all components first
        //refresh();
    }

    // Refresh the GUI components
    public void refresh() {
        // Reset the GUI components to default
        reset();
        try {
            // Get the user variables
            final String userAct = (String) mProject.getValueOf("UserAct").getValue();
            final String userRole = (String) mProject.getValueOf("UserRole").getValue();
            final String userState = (String) mProject.getValueOf("UserState").getValue();
            final String userVoice = (String) mProject.getValueOf("UserVoice").getValue();
            // get the agent variables
            final String agentAct = (String) mProject.getValueOf("AgentAct").getValue();
            final String agentRole = (String) mProject.getValueOf("AgentRole").getValue();
            final String agentState = (String) mProject.getValueOf("AgentState").getValue();
            final String agentVoice = (String) mProject.getValueOf("AgentVoice").getValue();

            // Refresh the text of the labels
            mUserStateLabel.setText("State: " + userState);
            mAgentStateLabel.setText("State: " + agentState);
            mUserRoleLabel.setText("Role: " + userRole);
            mAgentRoleLabel.setText("Role: " + agentRole);
            mUserActLabel.setText("Action: " + userAct);
            mAgentActLabel.setText("Action: " + agentAct);

            // Refresh the text of the buttons
            if (userState.equals("absent")) {
                mUserStateButton.setText("Enter");
            } else {
                mUserStateButton.setText("Leave");
            }

            if (agentState.equals("absent")) {
                mAgentStateButton.setText("Enter");
            } else {
                mAgentStateButton.setText("Leave");
            }

            if (userVoice.equals("speaking")) {
                mUserVoiceButton.setText("Stop");
            } else {
                mUserVoiceButton.setText("Start");
            }

            // Refresh the state of the buttons
            if (userState.equals("present")
                    && agentState.equals("present")) {
                // Enable the user input
                mAffectLabel.setEnabled(true);
                mAffectSlider.setEnabled(true);
                mEyegazeLabel.setEnabled(true);
                mEyegazeCombo.setEnabled(true);
                mExpressionLabel.setEnabled(true);
                mExpressionCombo.setEnabled(true);
                mUserVoiceLabel.setEnabled(true);
                mUserVoiceButton.setEnabled(true);
                //
                mScriptLabel.setEnabled(true);
                mScriptCombo.setEnabled(true);
                //
                if (userRole.equals("speaker")
                        && agentRole.equals("addressee")) {
                    //
                    if (userAct.equals("offering")) {
                        mAgentAcceptButton.setEnabled(true);
                        mAgentRejectButton.setEnabled(true);
                    } else if (agentAct.equals("requesting")) {
                        mUserLeaveButton.setEnabled(true);
                        mUserClaimButton.setEnabled(true);
                    } else {
                        mAgentRequestButton.setEnabled(true);
                        mUserOfferButton.setEnabled(true);
                    }
                } else if (userRole.equals("addressee")
                        && agentRole.equals("speaker")) {
                    //
                    if (userAct.equals("requesting")) {
                        mAgentLeaveButton.setEnabled(true);
                        mAgentClaimButton.setEnabled(true);
                    } else if (agentAct.equals("offering")) {
                        mUserAcceptButton.setEnabled(true);
                        mUserRejectButton.setEnabled(true);
                    } else {
                        mUserRequestButton.setEnabled(true);
                        mAgentOfferButton.setEnabled(true);
                    }
                }
            }
        } catch (final NullPointerException exc) {
            mLogger.failure(exc.toString());
                  
        }
    }

    private void reset() {
        // User status variables
        mUserRoleLabel.setEnabled(false);
        mUserActLabel.setEnabled(false);
        // User state button
        mUserStateButton.setEnabled(true);
        // Regulation buttons
        mUserRequestButton.setEnabled(false);
        mUserAcceptButton.setEnabled(false);
        mUserRejectButton.setEnabled(false);
        mUserOfferButton.setEnabled(false);
        mUserLeaveButton.setEnabled(false);
        mUserClaimButton.setEnabled(false);
        //
        mAffectLabel.setEnabled(false);
        mAffectSlider.setEnabled(false);
        mEyegazeLabel.setEnabled(false);
        mEyegazeCombo.setEnabled(false);
        mExpressionLabel.setEnabled(false);
        mExpressionCombo.setEnabled(false);
        // User voice button
        mUserVoiceLabel.setEnabled(false);
        mUserVoiceButton.setEnabled(false);
        // Agent status variables
        mAgentRoleLabel.setEnabled(false);
        mAgentActLabel.setEnabled(false);
        // Agent state button
        mAgentStateButton.setEnabled(true);
        //mAgentVoiceButton.setEnabled(false);
        mAgentRequestButton.setEnabled(false);
        mAgentAcceptButton.setEnabled(false);
        mAgentRejectButton.setEnabled(false);
        mAgentOfferButton.setEnabled(false);
        mAgentLeaveButton.setEnabled(false);
        mAgentClaimButton.setEnabled(false);
        //
        mScriptLabel.setEnabled(false);
        mScriptCombo.setEnabled(false);
    }

    // Show the agent wizard
    public final void show() {
        mMainWizardFrame.setVisible(true);
    }

    // Hide the agent wizard
    public final void hide() {
        mMainWizardFrame.setVisible(false);
    }

    // Kill the agent wizard
    public final void kill() {
        // Dispose the main frame
        mMainWizardFrame.dispose();
    }

    // Add text to the output console
    public void output(final String text) {
        // Format the date
        final String date = mFormat.format(new Date());
        // Get the document
        final StyledDocument doc = mOutputWizardArea.getStyledDocument();
        try {
            doc.insertString(0, "\r\n", null);
            doc.insertString(0, text, mTextStyle);
            doc.insertString(0, ":\r\n", null);
            doc.insertString(0, date, mDateStyle);
        } catch (final Exception exc) {
            // Do nothing
        }
        // Set the caret positio to the top
        mOutputWizardArea.setCaretPosition(0);
    }

    // Assert a fact to the fact base
    public void insert(final String fact) {
        // Assert the fact to the fact base 
        //mPlayer.query("jdd(" + fact + ").");
    }
}
