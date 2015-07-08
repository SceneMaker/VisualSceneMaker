package de.dfki.vsm.api.hcm;

import de.dfki.vsm.api.VSMAgentClient;
import de.dfki.vsm.api.VSMScenePlayer;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.model.scenescript.AbstractWord;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.runtime.Process;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.StructValue;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public final class HCMScenePlayer extends VSMScenePlayer {

    // The Singelton Player
    public static HCMScenePlayer sInstance;

    // The Feedback Game UI
    private HCMUserInterface mUserInterface;

    // The HCM Event Handler
    private HCMEventHandler mEventHandler;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private HCMScenePlayer(
            final RunTimeProject project,
            final PlayerConfig config) {

        // Initialize The Scene Player
        super(project, config);

        // Print Some Debug Information
        mVSM3Log.message("Creating HCM Scene Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static synchronized HCMScenePlayer getInstance(
            final RunTimeProject project,
            final PlayerConfig config) {
        if (sInstance == null) {
            sInstance = new HCMScenePlayer(project, config);
        }

        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void launch() {

        // Load Parent Scene Player
        super.launch();

        // Initialize Scene Player
        try {

            // Initialize Event Handler
            final String lhost = mPlayerConfig.getProperty("hcm.handler.local.host");
            final String rhost = mPlayerConfig.getProperty("hcm.handler.remote.host");
            final String lport = mPlayerConfig.getProperty("hcm.handler.local.port");
            final String rport = mPlayerConfig.getProperty("hcm.handler.remote.port");
            final String rflag = mPlayerConfig.getProperty("hcm.handler.remote.flag");

            // Debug Some Information
            mVSM3Log.message("" + "HCM Event Handler Local Host  : '" + lhost + "'" + "\r\n"
                    + "HCM Event Handler Local Port  : '" + lport + "'" + "\r\n"
                    + "HCM Event Handler Remote Host : '" + rhost + "'" + "\r\n"
                    + "HCM Event Handler Remote Port : '" + rport + "'" + "\r\n"
                    + "HCM Event Handler Remote Flag : '" + rflag + "'");

            // Construct Event Handler
            mEventHandler = new HCMEventHandler(this);

            // Initialize Event Handler
            mEventHandler.init(lhost, Integer.parseInt(lport), rhost, Integer.parseInt(rport), Boolean.valueOf(rflag));

            // Start Event Handler Now
            mEventHandler.start();
        } catch (final NumberFormatException exc) {

            // Debug Some Information
            mVSM3Log.failure(exc.toString());

            // Debug Some Information
            mVSM3Log.message("Failed Launching HCM Scene Player");
        }

        // Create User Interface
        mUserInterface = new HCMUserInterface(this);

        // Print Debug Information
        mVSM3Log.message("Launching HCM Scene Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void unload() {

        // Unload Parent Scene Player
        super.unload();

        // Dispose The User Interface
        mUserInterface.dispose();

        // Abort The Event Handler
        mEventHandler.abort();

        // Join With All Threads
        try {

            // Join With Event Handler
            mEventHandler.join();

            // Print Debug Information
            mVSM3Log.message("Joining HCM Event Handler");
        } catch (Exception exc) {

            // Print Debug Information
            mVSM3Log.warning(exc.toString());
        }

        // Print Debug Information
        mVSM3Log.message("Unloading HCM Scene Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void hideGUI() {
        if (mUserInterface != null) {
            mUserInterface.setVisible(false);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void showGUI() {
        if (mUserInterface != null) {
            mUserInterface.setVisible(true);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void handle(final VSMAgentClient client) {

        // Receive The Next Incoming String
        final String input = client.recvString();

        // Check The Incoming Data String
        if (input != null) {

            // Debug Some Information
            mVSM3Log.message("Agent #" + client.getAgentUaid() + " Receiving Data '" + input + "'");

            // Parse The Data To A New Message
            final HCMEventMessage message = HCMEventMessage.getInstance(input);

            // Find The Message Task Identifier
            if (message.getUtid() != null) {

                // Get The Message Task Identifier
                final String utid = message.getUtid();

                // Notify The Right Waiting Thread
                synchronized (mWaitingThreadQueue) {

                    // Remove Respective Task
                    final Thread task = mWaitingThreadQueue.remove(utid);

                    // Debug Some Information
                    mVSM3Log.message("Removing The Waiting Thread '" + task + "' With Id '" + utid + "'");

                    // Debug Some Information
                    mVSM3Log.message("Notifying All Waiting Threads");

                    // Notify Waiting Tasks
                    mWaitingThreadQueue.notifyAll();
                }
            } else {

                // Debug Some Information
                mVSM3Log.warning("Message Has Wrong Format '" + input + "'");
            }
        } else {

            // Debug Some Information
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void request(final String type, final String utid, final String name, final String uaid,
            final String text, final long date, final long time, final Thread task,
            final VSMAgentClient client) {

        // Construct Event Message
        final HCMEventMessage message = HCMEventMessage.newInstance(type, utid, name, uaid, text, date, time);

        // Send The New Command
        synchronized (mWaitingThreadQueue) {
            if (client.sendString(message.toString())) {

                // Print Information
                mVSM3Log.message("Enqueuing Waiting Thread '" + task + "' With Id '" + utid + "'");

                // Enqueue The Waiting Thread
                mWaitingThreadQueue.put(utid, Thread.currentThread());

                // And Await The Notification
                while (mWaitingThreadQueue.containsKey(utid)) {

                    // Print Information
                    mVSM3Log.message("Awaiting Notification For '" + task + "' With Id '" + utid + "'");

                    // Print Information
                    try {
                        mWaitingThreadQueue.wait();
                    } catch (final InterruptedException exc) {

                        // Print Information
                        mVSM3Log.message("Interrupting Waiting Task '" + task + "' With Id '" + utid + "'");
                    }

                    // Print Information
                    mVSM3Log.message("Searching Notification For '" + task + "' With Id '" + utid + "'");
                }

                // Print Information
                mVSM3Log.message("Received Notification For '" + task + "' With Id '" + utid + "'");
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void play(final String sceneName, final LinkedList<AbstractValue> sceneArgs) {

        // Initialize Parameter Data
        final Process process = (Process) Thread.currentThread();
        final String utid = process.getName() + "::" + sceneName;

        // And Process Scene Arguments
        final HashMap<String, String> argMap = new HashMap<>();

        if ((sceneArgs != null) && !sceneArgs.isEmpty()) {
            final AbstractValue value = sceneArgs.getFirst();

            if (value.getType().equals(AbstractValue.Type.STRUCT)) {
                final StructValue struct = ((StructValue) value);

                for (final Map.Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
                    if (entry.getValue().getType() == AbstractValue.Type.STRING) {
                        final StringValue string = (StringValue) entry.getValue();

                        argMap.put(entry.getKey(), string.getValue());
                    }
                }
            }
        }

        // Create The Player Task
        Task task = new Task(utid) {
            @Override
            public void run() {

                // Select A Scene From Script
                final SceneScript sceneScript = mProjectData.getSceneScript();

                // Select Default Scene Group
                final SceneGroup sceneGroup = sceneScript.getSceneGroup(sceneName);

                // Select Language Dependent Group
                final SceneGroup langGroup = sceneScript.getSceneGroup("de", sceneName);

                // Select A Scene From Group
                final SceneObject selectedScene = (langGroup != null)
                        ? (langGroup.select())
                        : (sceneGroup.select());

                // Infer The Final Scene Language
                // final String selectedLang = selectedScene.getLanguage();
                // Process The Scene Turns
                for (final SceneTurn sceneTurn : selectedScene.getTurnList()) {

                    // Process The Utterances Of This Turn Here
                    // Process Turn With Both, Text And Actions
                    for (final SceneUttr sceneUtt : sceneTurn.getUttrList()) {

                        // Create An Action Text Builder
                        final StringBuilder textBuilder = new StringBuilder();

                        // Process The Words Of The Utterance
                        for (final AbstractWord sceneWord : sceneUtt.getWordList()) {

                            // Continue With Next
                            textBuilder.append(sceneWord.getText(argMap));

                            // Append Whitespace
                            if (!sceneWord.equals(sceneUtt.getWordList().get(sceneUtt.getWordList().size() - 1))) {
                                textBuilder.append(' ');
                            }
                        }

                        // Check Action Builder Length
                        if (textBuilder.length() > 0) {

                            // Append Punctation MArk
                            textBuilder.append(sceneUtt.getPunct());
                        }

                        // Get The Speaker Name
                        final String name = sceneTurn.getSpeaker();

                        // Check if The Speaker Exists
                        if (name != null) {

                            // Get The Adequate Agent
                            final VSMAgentClient client = mAgentClientMap.get(name);

                            // Check if The Client Exists
                            if (client != null) {

                                // Get The Current Thread Object
                                final Thread thread = (Thread) this;

                                // Get The Current Player Time
                                final long time = getCurrentTime();

                                // Get The Agent Unique Id
                                final String uaid = client.getAgentUaid();

                                // Get The Current System Time
                                final long date = System.currentTimeMillis();

                                // Get the Message Text Content
                                final String text = textBuilder.toString();

                                // Request Command Execution
                                request("utterance", utid, name, uaid, text, date, time, thread, client);
                            } else {

                                // Get The Default Speaker
                                mVSM3Log.failure("Agent '" + name + "'Does Not Exist");
                            }
                        } else {

                            // Get The Default Speaker
                            mVSM3Log.failure("Turn Has No Speaker Definition");
                        }

                        // Exit If Interrupted After Utterance
                        if (mIsDone) {

                            // Print Information
                            mVSM3Log.message("Finishing Task '" + utid + "' After Utterance");

                            return;
                        }
                    }

                    // Exit If Interrupted After Turn
                    if (mIsDone) {

                        // Print Information
                        mVSM3Log.message("Finishing Task '" + utid + "' After Turn");

                        return;
                    }
                }
            }
        };

        // Start The Player Task
        task.start();

        // Wait For Termination
        boolean finished = false;

        while (!finished) {
            try {

                // Join The Player Task
                task.join();

                // Finish This Execution
                finished = true;
            } catch (InterruptedException exc) {

                // Print Information
                mVSM3Log.warning("Interrupting '" + Thread.currentThread().getName()
                        + "' During The Execution Of Scene '" + sceneName + "'");

                // Terminate The Task
                task.mIsDone = true;

                // Interrupt The Task
                task.interrupt();
            }
        }
    }
}
