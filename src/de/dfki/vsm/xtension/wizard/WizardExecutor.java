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
import de.dfki.vsm.util.jpl.JPLEngine;
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
import javax.swing.JTextPane;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.BoxLayout;
import static javax.swing.BoxLayout.X_AXIS;
import static javax.swing.BoxLayout.Y_AXIS;
import javax.swing.JComponent;
import javax.swing.JTextField;
import javax.swing.text.BadLocationException;

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
    private JPanel mAgentStatusPanel;
    private JPanel mUserInputPanel;
    private JPanel mAgentInputPanel;
    // Output panel
    private JPanel mOutputWizardPanel;
    private JTextPane mOutputWizardArea;
    private JScrollPane mOutputWizardPane;
    // Status labels
    private JLabel mUserStateLabel;
    private JLabel mUserRoleLabel;
    private JLabel mUserActionLabel;
    private JLabel mAgentStateLabel;
    private JLabel mAgentRoleLabel;
    private JLabel mAgentActionLabel;
    // Status buttons
    private JButton mUserStateButton;
    private JButton mAgentStateButton;
    // User voice
    private JLabel mUserVoiceLabel;
    private JButton mUserVoiceButton;
    // User touch
    private JLabel mUserTouchLabel;
    private JButton mUserStartButton;
    private JButton mUserDragButton;
    private JButton mUserStopButton;
    // User Speech
    private JLabel mUserSpeechLabel;
    private JTextField mUserSpeechField;
    // Agent Scripts
    private JLabel mAgentScriptLabel;
    private JButton mAgentScriptButton;
    private JLabel mAgentComboLabel;
    private JComboBox mAgentScriptCombo;
    private DefaultComboBoxModel mScriptModel;
    //
    private JLabel mUserElicitLabel;
    private JButton mUserElicitButton;
    // User expressions
    private JLabel mUserExpLabel;
    private JComboBox mUserExpCombo;
    private DefaultComboBoxModel mUserEmoModel;
    // User eyegaze
    private JLabel mUserGazeLabel;
    private JComboBox mUserGazeCombo;
    private DefaultComboBoxModel mUserGazeModel;
    // Expressions
    private JLabel mAgentExpLabel;
    private JComboBox mAgentExpCombo;
    private DefaultComboBoxModel mAgentEmoModel;
    // Eyegaze
    private JLabel mAgentGazeLabel;
    private JComboBox mAgentGazeCombo;
    private DefaultComboBoxModel mAgentGazeModel;
    //
    private JLabel mAgentBackLabel;
    private JButton mAgentBackButton;

    // Style attributes
    private SimpleAttributeSet mDateStyle;
    private SimpleAttributeSet mTextStyle;
    // The scene script 
    private final SceneScript mSceneScript;
    // The date format
    private final SimpleDateFormat mFormat
            = new SimpleDateFormat("HH:mm:ss.SSS");
    //
    private GazeThread mUserGazeThread;
    private GazeThread mAgentGazeThread;

    // Launch the executor 
    @Override
    public void launch() {
        show();
        start();
    }

    // Unload the executor 
    @Override
    public void unload() {
        hide();
        stop();
        clear();
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
        if (activity instanceof ActionActivity) {
            final ActionActivity action = (ActionActivity) activity;
            if (name.equals("refresh")) {
                refresh();
            } else if (name.equals("output")) {
                if (features.get(0).getKey().equals("text")) {
                    output(features.get(0).getVal());
                }
            }
        }
    }

    // Create a new agent wizard
    public WizardExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        // Initialize scene script
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
        // Create the emotion models
        mUserGazeModel = new DefaultComboBoxModel();
        mAgentGazeModel = new DefaultComboBoxModel();
        // Add the emotion expressions
        mUserGazeModel.addElement("pen");
        mUserGazeModel.addElement("cup");
        mUserGazeModel.addElement("book");
        mUserGazeModel.addElement("agent");
        //
        mAgentGazeModel.addElement("pen");
        mAgentGazeModel.addElement("cup");
        mAgentGazeModel.addElement("book");
        mAgentGazeModel.addElement("user");
        // Deselect everything first
        mUserGazeModel.setSelectedItem("agent");
        mAgentGazeModel.setSelectedItem("user");
    }

    // Initialize the expression box
    private void loadEmotionExps() {
        // Create the emotion models
        mUserEmoModel = new DefaultComboBoxModel();
        mAgentEmoModel = new DefaultComboBoxModel();
        // Add the emotion expressions
        mUserEmoModel.addElement("neutral");
        mUserEmoModel.addElement("joy");
        mUserEmoModel.addElement("sad");
        mUserEmoModel.addElement("fear");
        mUserEmoModel.addElement("anger");
        mUserEmoModel.addElement("disgust");
        mUserEmoModel.addElement("surprise");
        //
        mAgentEmoModel.addElement("neutral");
        mAgentEmoModel.addElement("joy");
        mAgentEmoModel.addElement("sad");
        mAgentEmoModel.addElement("fear");
        mAgentEmoModel.addElement("anger");
        mAgentEmoModel.addElement("disgust");
        mAgentEmoModel.addElement("surprise");
        // Deselect everything first
        mUserEmoModel.setSelectedItem("neutral");
        mAgentEmoModel.setSelectedItem("neutral");
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
                final boolean userPresent = (boolean) mProject.getValueOf("UserPresent").getValue();
                if (userPresent) {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "user" + "," + "\n"
                            + "mode:" + "state" + "," + "\n"
                            + "data:" + "leave" + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                } else {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "user" + "," + "\n"
                            + "mode:" + "state" + "," + "\n"
                            + "data:" + "enter" + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                }
            }
        });
        mUserActionLabel = new JLabel("Action:");
        mUserRoleLabel = new JLabel("Role:");
        mUserStateLabel = new JLabel("Status:");

        sanitize(mUserStateLabel);
        sanitize(mUserStateButton);
        sanitize(mUserRoleLabel);
        sanitize(mUserActionLabel);

        mUserStatusPanel = new JPanel();
        mUserStatusPanel.setLayout(new GridLayout(2, 2));
        mUserStatusPanel.setBorder(BorderFactory.createTitledBorder("Status Variables"));
        mUserStatusPanel.add(mUserStateLabel);
        mUserStatusPanel.add(mUserStateButton);
        mUserStatusPanel.add(mUserRoleLabel);
        mUserStatusPanel.add(mUserActionLabel);

        // Create the user verbal panel ////////////////////////////////////////
        mUserVoiceLabel = new JLabel("Voice Activity:");
        mUserVoiceLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserVoiceButton = new JButton();
        mUserVoiceButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean userSpeaking = (boolean) mProject.getValueOf("UserSpeaking").getValue();
                if (userSpeaking) {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "user" + "," + "\n"
                            + "mode:" + "voice" + "," + "\n"
                            + "data:" + "stop" + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                } else {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "user" + "," + "\n"
                            + "mode:" + "voice" + "," + "\n"
                            + "data:" + "start" + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                }
            }
        });

        // Create the user verbal panel ////////////////////////////////////////
        mUserTouchLabel = new JLabel("Touch Activity:");
        mUserTouchLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserStartButton = new JButton("Start");
        mUserStartButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                JPLEngine.query("now(Time), "
                        + "jdd(["
                        + "type:" + "event" + "," + "\n"
                        + "name:" + "user" + "," + "\n"
                        + "mode:" + "touch" + "," + "\n"
                        + "data:" + "["
                        + "  type:" + "start" + ","
                        + "  name:" + "p1" + ","
                        + "  pos:" + "[x:0, y:0]"
                        + "]" + "," + "\n"
                        + "time:" + "Time" + "," + "\n"
                        + "dist:" + 0 + "," + "\n"
                        + "life:" + 0 + "," + "\n"
                        + "conf:" + 1.0 + "\n"
                        + "]).");
            }
        });
        mUserDragButton = new JButton("Drag");
        mUserDragButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                JPLEngine.query("now(Time), "
                        + "jdd(["
                        + "type:" + "event" + "," + "\n"
                        + "name:" + "user" + "," + "\n"
                        + "mode:" + "touch" + "," + "\n"
                        + "data:" + "["
                        + "  type:" + "drag" + ","
                        + "  name:" + "p1" + ","
                        + "  pos:" + "[x:0, y:0]"
                        + "]" + "," + "\n"
                        + "time:" + "Time" + "," + "\n"
                        + "dist:" + 0 + "," + "\n"
                        + "life:" + 0 + "," + "\n"
                        + "conf:" + 1.0 + "\n"
                        + "]).");
            }
        });
        mUserStopButton = new JButton("Stop");
        mUserStopButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                JPLEngine.query("now(Time), "
                        + "jdd(["
                        + "type:" + "event" + "," + "\n"
                        + "name:" + "user" + "," + "\n"
                        + "mode:" + "touch" + "," + "\n"
                        + "data:" + "["
                        + "  type:" + "stop" + ","
                        + "  name:" + "p1" + ","
                        + "  pos:" + "[x:0, y:0]"
                        + "]" + "," + "\n"
                        + "time:" + "Time" + "," + "\n"
                        + "dist:" + 0 + "," + "\n"
                        + "life:" + 0 + "," + "\n"
                        + "conf:" + 1.0 + "\n"
                        + "]).");
            }
        });

        mUserSpeechLabel = new JLabel("Utterance:");
        mUserSpeechLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserSpeechField = new JTextField();
        mUserSpeechField.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent event) {

                final String text = mUserSpeechField.getText().trim();
                if (!text.isEmpty()) {
                    JPLEngine.query("now(Time), "
                            + "signal(speech, dialog,"
                            + "["
                            + "  type:" + "dialog_act" + ","
                            + "  fun:" + "info_seeking" + ","
                            + "  cat:" + "check_question" + ","
                            + "  data:" + "["
                            + "    size:" + "large" + ","
                            + "    color:" + "yellow" + ","
                            + "    shape:" + "square" + ","
                            + "    loc:" + (text.isEmpty() ? "[ ]" : text)
                            + "  ]"
                            + "]).");
                }
            }
        });

        mUserExpLabel = new JLabel("Expression:");
        mUserExpLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserExpCombo = new JComboBox(mUserEmoModel);
        mUserExpCombo.setBackground(Color.WHITE);
        mUserExpCombo.setBorder(BorderFactory.createEtchedBorder());
        mUserExpCombo.addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(final ItemEvent event) {
                JPLEngine.query("now(Time), "
                        + "jdd(["
                        + "type:" + "event" + "," + "\n"
                        + "name:" + "user" + "," + "\n"
                        + "mode:" + "facs" + "," + "\n"
                        + "data:" + ((String) event.getItem()) + "," + "\n"
                        + "time:" + "Time" + "," + "\n"
                        + "dist:" + 0 + "," + "\n"
                        + "life:" + 0 + "," + "\n"
                        + "conf:" + 1.0 + "\n"
                        + "]).");
            }
        });
        mUserGazeLabel = new JLabel("Eyegaze:");
        mUserGazeLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserGazeCombo = new JComboBox(mUserGazeModel);
        mUserGazeCombo.setBackground(Color.WHITE);
        mUserGazeCombo.setBorder(BorderFactory.createEtchedBorder());
       

        mUserElicitLabel = new JLabel("Head Nod:");
        mUserElicitLabel.setBorder(BorderFactory.createEmptyBorder());
        mUserElicitButton = new JButton("Produce");
        mUserElicitButton.setBackground(Color.WHITE);
        mUserElicitButton.setBorder(BorderFactory.createEmptyBorder());
        mUserElicitButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                 JPLEngine.query("now(Time), "
                        + "jdd(["
                        + "type:" + "event" + "," + "\n"
                        + "name:" + "user" + "," + "\n"
                        + "mode:" + "head" + "," + "\n"
                        + "data:" + "nod" + "," + "\n"
                        + "time:" + "Time" + "," + "\n"
                        + "dist:" + 0 + "," + "\n"
                        + "life:" + 0 + "," + "\n"
                        + "conf:" + 1.0 + "\n"
                        + "]).");
            }
        });

        sanitize(mUserVoiceLabel);
        sanitize(mUserVoiceButton);
        sanitize(mUserSpeechLabel);
        sanitize(mUserSpeechField);
        sanitize(mUserExpLabel);
        sanitize(mUserExpCombo);
        sanitize(mUserGazeLabel);
        sanitize(mUserGazeCombo);
        sanitize(mUserElicitLabel);
        sanitize(mUserElicitButton);

        mUserInputPanel = new JPanel();
        mUserInputPanel.setLayout(new GridLayout(7, 2));
        mUserInputPanel.setBorder(BorderFactory.createEmptyBorder());
        mUserInputPanel.add(mUserVoiceLabel);
        mUserInputPanel.add(mUserVoiceButton);
        mUserInputPanel.add(mUserTouchLabel);
        mUserInputPanel.add(mUserDragButton);
        mUserInputPanel.add(mUserStartButton);
        mUserInputPanel.add(mUserStopButton);
        mUserInputPanel.add(mUserSpeechLabel);
        mUserInputPanel.add(mUserSpeechField);
        mUserInputPanel.add(mUserExpLabel);
        mUserInputPanel.add(mUserExpCombo);
        mUserInputPanel.add(mUserGazeLabel);
        mUserInputPanel.add(mUserGazeCombo);
        mUserInputPanel.add(mUserElicitLabel);
        mUserInputPanel.add(mUserElicitButton);

        // Create the agent status panel ///////////////////////////////////////
        mAgentStateButton = new JButton();
        mAgentStateButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                final boolean agentAttendant = (boolean) mProject.getValueOf("AgentPresent").getValue();
                if (agentAttendant) {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "agent" + "," + "\n"
                            + "mode:" + "state" + "," + "\n"
                            + "data:" + "leave" + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                } else {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "agent" + "," + "\n"
                            + "mode:" + "state" + "," + "\n"
                            + "data:" + "enter" + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                }
            }
        });
        mAgentActionLabel = new JLabel("Action:");
        mAgentRoleLabel = new JLabel("Role:");
        mAgentStateLabel = new JLabel("Status:");

        sanitize(mAgentStateLabel);
        sanitize(mAgentStateButton);
        sanitize(mAgentRoleLabel);
        sanitize(mAgentActionLabel);

        mAgentStatusPanel = new JPanel();
        mAgentStatusPanel.setLayout(new GridLayout(2, 2));
        mAgentStatusPanel.setBorder(BorderFactory.createTitledBorder("Status Variables"));
        mAgentStatusPanel.add(mAgentStateLabel);
        mAgentStatusPanel.add(mAgentStateButton);
        mAgentStatusPanel.add(mAgentRoleLabel);
        mAgentStatusPanel.add(mAgentActionLabel);

        // Create the agent input panel ///////////////////////////////////////
        mAgentScriptLabel = new JLabel("Play Scene:");
        mAgentScriptLabel.setBorder(BorderFactory.createEmptyBorder());
        mAgentScriptButton = new JButton("Try Contribution");
        mAgentScriptButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                final String scene = (String) mAgentScriptCombo.getSelectedItem();
                JPLEngine.query("now(Time), "
                        + "signal(planner, dialog,"
                        + "["
                        + "dialogscene:" + scene + ","
                        + "abortscene:" + "abort" + ","
                        + "addressee:" + "agent"
                        + "]).");

            }
        });

        mAgentComboLabel = new JLabel("");
        mAgentComboLabel.setBorder(BorderFactory.createEmptyBorder());
        mAgentScriptCombo = new JComboBox(mScriptModel);
        mAgentScriptCombo.setBackground(Color.WHITE);
        mAgentScriptCombo.setBorder(BorderFactory.createEtchedBorder());

        mAgentExpLabel = new JLabel("Expression:");
        mAgentExpLabel.setBorder(BorderFactory.createEmptyBorder());
        mAgentExpCombo = new JComboBox(mAgentEmoModel);
        mAgentExpCombo.setBackground(Color.WHITE);
        mAgentExpCombo.setBorder(BorderFactory.createEtchedBorder());
        mAgentExpCombo.addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(final ItemEvent event) {
                if (event.getStateChange() == ItemEvent.SELECTED) {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + "," + "\n"
                            + "name:" + "agent" + "," + "\n"
                            + "mode:" + "facs" + "," + "\n"
                            + "data:" + ((String) event.getItem()) + "," + "\n"
                            + "time:" + "Time" + "," + "\n"
                            + "dist:" + 0 + "," + "\n"
                            + "life:" + 0 + "," + "\n"
                            + "conf:" + 1.0 + "\n"
                            + "]).");
                }
            }
        });

        mAgentGazeLabel = new JLabel("Eyegaze:");
        mAgentGazeLabel.setBorder(BorderFactory.createEmptyBorder());
        mAgentGazeCombo = new JComboBox(mAgentGazeModel);
        mAgentGazeCombo.setBackground(Color.WHITE);
        mAgentGazeCombo.setBorder(BorderFactory.createEtchedBorder());
        //
        /*
         mAgentGazeCombo.addItemListener(new ItemListener() {

         @Override
         public void itemStateChanged(final ItemEvent event) {
         if (event.getStateChange() == ItemEvent.SELECTED) {
         JPLEngine.query("now(Time), "
         + "jdd(["
         + "type:" + "event" + "," + "\n"
         + "name:" + "agent" + "," + "\n"
         + "mode:" + "gaze" + "," + "\n"
         + "data:" + ((String) event.getItem()) + "," + "\n"
         + "time:" + "Time" + "," + "\n"
         + "dist:" + 0 + "," + "\n"
         + "life:" + 0 + "," + "\n"
         + "conf:" + 1.0 + "\n"
         + "]).");
         }
         }
         });
         */

        mAgentBackLabel = new JLabel("Backchannel:");
        mAgentBackLabel.setBorder(BorderFactory.createEmptyBorder());
        mAgentBackButton = new JButton("Produce");
        mAgentBackButton.setBackground(Color.WHITE);
        mAgentBackButton.setBorder(BorderFactory.createEmptyBorder());
        mAgentBackButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent event) {

            }
        });

        sanitize(mAgentScriptLabel);
        sanitize(mAgentScriptButton);
        sanitize(mAgentComboLabel);
        sanitize(mAgentScriptCombo);
        sanitize(mAgentExpLabel);
        sanitize(mAgentExpCombo);
        sanitize(mAgentGazeLabel);
        sanitize(mAgentGazeCombo);
        sanitize(mAgentBackLabel);
        sanitize(mAgentBackButton);

        mAgentInputPanel = new JPanel();
        mAgentInputPanel.setLayout(new GridLayout(5, 2));
        mAgentInputPanel.setBorder(BorderFactory.createEmptyBorder());
        mAgentInputPanel.add(mAgentScriptLabel);
        mAgentInputPanel.add(mAgentScriptButton);
        mAgentInputPanel.add(mAgentComboLabel);
        mAgentInputPanel.add(mAgentScriptCombo);
        mAgentInputPanel.add(mAgentExpLabel);
        mAgentInputPanel.add(mAgentExpCombo);
        mAgentInputPanel.add(mAgentGazeLabel);
        mAgentInputPanel.add(mAgentGazeCombo);
        mAgentInputPanel.add(mAgentBackLabel);
        mAgentInputPanel.add(mAgentBackButton);

        // Create the user wizard panel ////////////////////////////////////////
        mUserWizardPanel = new JPanel();
        mUserWizardPanel.setLayout(new BoxLayout(mUserWizardPanel, Y_AXIS));
        mUserWizardPanel.setBorder(BorderFactory.createTitledBorder("User Wizard"));
        mUserWizardPanel.add(mUserStatusPanel);
        mUserWizardPanel.add(mUserInputPanel);

        // Create the agent wizard panel ////////////////////////////////////////
        mAgentWizardPanel = new JPanel();
        mAgentWizardPanel.setLayout(new BoxLayout(mAgentWizardPanel, Y_AXIS));
        mAgentWizardPanel.setBorder(BorderFactory.createTitledBorder("Agent Wizard"));
        mAgentWizardPanel.add(mAgentStatusPanel);
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
        mOutputWizardPanel.setPreferredSize(new Dimension(600, 300));
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

    private void sanitize(final JComponent component) {
        final Dimension dim = new Dimension(100, 25);
        component.setPreferredSize(dim);
        component.setMaximumSize(dim);
        component.setMinimumSize(dim);
    }

    private void start() {
        mUserGazeThread = new GazeThread(1250, "user", mUserGazeModel);
        mAgentGazeThread = new GazeThread(1750, "agent", mAgentGazeModel);
        mUserGazeThread.start();
        mAgentGazeThread.start();
    }

    //
    private void stop() {
        mUserGazeThread.abort();
        mAgentGazeThread.abort();
        try {
            mUserGazeThread.join();
            mAgentGazeThread.join();
        } catch (final InterruptedException exc) {
            mLogger.failure(exc.toString());
        }
    }

    //
    private void clear() {
        //final StyledDocument doc = mOutputWizardArea.getStyledDocument();
        mOutputWizardArea.setText("");
    }

    // Refresh the GUI components
    public void refresh() {
        mLogger.message("Refreshing Wizard");
        // Reset the GUI components to default
        reset();
        try {
            // Get the user variables
            final String userRole = (String) mProject.getValueOf("UserRole").getValue();
            final String userAction = (String) mProject.getValueOf("UserAction").getValue();
            final boolean userPresent = (boolean) mProject.getValueOf("UserPresent").getValue();
            final boolean userSpeaking = (boolean) mProject.getValueOf("UserSpeaking").getValue();
            // get the agent variables
            final String agentRole = (String) mProject.getValueOf("AgentRole").getValue();
            final String agentAction = (String) mProject.getValueOf("AgentAction").getValue();
            final boolean agentPresent = (boolean) mProject.getValueOf("AgentPresent").getValue();
            final boolean agentSpeaking = (boolean) mProject.getValueOf("AgentSpeaking").getValue();

            // Refresh the text of the labels
            mUserStateLabel.setText("State: " + (userPresent ? "Present" : "Absent"));
            mAgentStateLabel.setText("State: " + (agentPresent ? "Present" : "Absent"));
            mUserRoleLabel.setText("Role: " + userRole);
            mAgentRoleLabel.setText("Role: " + agentRole);
            mUserActionLabel.setText("Action: " + userAction);
            mAgentActionLabel.setText("Action: " + agentAction);
            //mAgentVoiceLabel.setText("Voice Activity: " + agentSpeaking);

            // Refresh the text of the buttons
            if (userPresent) {
                mUserStateButton.setText("Leave");
            } else {
                mUserStateButton.setText("Enter");
            }
            if (agentPresent) {
                mAgentStateButton.setText("Leave");
            } else {
                mAgentStateButton.setText("Enter");
            }

            // Refresh the state of the buttons
            if (userPresent && agentPresent) {
                // 
                mUserVoiceLabel.setEnabled(true);
                mUserVoiceButton.setEnabled(true);
                mUserSpeechLabel.setEnabled(true);
                mUserSpeechField.setEnabled(true);
                mUserExpLabel.setEnabled(true);
                mUserExpCombo.setEnabled(true);
                mUserGazeLabel.setEnabled(true);
                mUserGazeCombo.setEnabled(true);
                mUserElicitLabel.setEnabled(true);
                mUserElicitButton.setEnabled(true);
                // 
                mAgentScriptButton.setEnabled(true);
                mAgentScriptLabel.setEnabled(true);
                mAgentComboLabel.setEnabled(true);
                mAgentScriptCombo.setEnabled(true);
                mAgentExpLabel.setEnabled(true);
                mAgentExpCombo.setEnabled(true);
                mAgentGazeLabel.setEnabled(true);
                mAgentGazeCombo.setEnabled(true);
                mAgentBackLabel.setEnabled(true);
                mAgentBackButton.setEnabled(true);
                //
                if (userRole.equals("speaker")
                        && agentRole.equals("addressee")) {
                    //                    
                } else if (userRole.equals("addressee")
                        && agentRole.equals("speaker")) {
                    //                    
                } else if (userRole.equals("bystander")
                        && agentRole.equals("bystander")) {
                    //
                }

                if (userSpeaking) {
                    mUserVoiceButton.setText("Stop");
                } else {
                    mUserVoiceButton.setText("Start");
                }
                if (agentSpeaking) {
                    mAgentScriptButton.setEnabled(false);
                } else {
                    mAgentScriptButton.setEnabled(true);
                }
            }
        } catch (final NullPointerException exc) {
            mLogger.failure(exc.toString());
        }
    }

    private void reset() {
        // 
        mUserRoleLabel.setEnabled(false);
        mUserActionLabel.setEnabled(false);
        mUserStateButton.setEnabled(true);
        //
        mUserVoiceLabel.setEnabled(false);
        mUserVoiceButton.setEnabled(false);
        mUserSpeechLabel.setEnabled(false);
        mUserSpeechField.setEnabled(false);
        mUserGazeLabel.setEnabled(false);
        mUserGazeCombo.setEnabled(false);
        mUserExpLabel.setEnabled(false);
        mUserExpCombo.setEnabled(false);
        mUserElicitLabel.setEnabled(false);
        mUserElicitButton.setEnabled(false);
        //
        mAgentRoleLabel.setEnabled(false);
        mAgentActionLabel.setEnabled(false);
        mAgentStateButton.setEnabled(true);
        //
        mAgentScriptLabel.setEnabled(false);
        mAgentScriptButton.setEnabled(false);
        mAgentComboLabel.setEnabled(false);
        mAgentScriptCombo.setEnabled(false);
        //
        mAgentGazeLabel.setEnabled(false);
        mAgentGazeCombo.setEnabled(false);
        mAgentExpLabel.setEnabled(false);
        mAgentExpCombo.setEnabled(false);
        mAgentBackLabel.setEnabled(false);
        mAgentBackButton.setEnabled(false);

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
        mLogger.message(text);
        // Format the date
        final String date = mFormat.format(new Date());
        // Get the document
        final StyledDocument doc = mOutputWizardArea.getStyledDocument();
        try {
            doc.insertString(0, "\r\n", null);
            doc.insertString(0, text, mTextStyle);
            doc.insertString(0, ":\r\n", null);
            doc.insertString(0, date, mDateStyle);
        } catch (final BadLocationException exc) {
            // Do nothing
        }
        // Set the caret positio to the top
        mOutputWizardArea.setCaretPosition(0);
    }

    private final class GazeThread extends Thread {

        private boolean mDone = false;
        private final long mTime;
        private final String mName;
        private final DefaultComboBoxModel mModel;

        public GazeThread(
                final long time,
                final String name,
                final DefaultComboBoxModel model) {
            mTime = time;
            mName = name;
            mModel = model;

        }

        public final void abort() {
            mDone = true;
            interrupt();
        }

        @Override
        public final void run() {

            while (!mDone) {
                try {
                    // Sleep some time
                    Thread.sleep(mTime);
                    // Get selected item
                    final String item = (String) mModel.getSelectedItem();
                    final String type = ((item.equals("user") || item.equals("agent")) ? "partner" : "entity");
                    final String sort = (item.equals("user") ? "user" : (item.equals("agent") ? "agent" : "piece"));

                    if (item != null) {
                        // Add new gaze event
                        JPLEngine.query("now(Time), "
                                + "jdd(["
                                + "type:" + "event" + "," + "\n"
                                + "name:" + mName + "," + "\n"
                                + "mode:" + "gaze" + "," + "\n"
                                + "data:" + "[" + "\n"
                                + "    type:" + type + ","
                                + "    sort:" + sort + ","
                                + "    name:" + item + ","
                                + "    conf:" + 1.0 + "\n"
                                + "]" + "," + "\n"
                                + "time:" + "Time" + "," + "\n"
                                + "dist:" + 0 + "," + "\n"
                                + "life:" + 0 + "," + "\n"
                                + "conf:" + 1.0 + "\n"
                                + "]).");
                    }
                } catch (final InterruptedException exc) {
                    mLogger.warning(exc.toString());
                }
            }
        }
    };

}
