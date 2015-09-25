 package de.dfki.vsm.api.hcm;

import de.dfki.vsm.api.VSMAgentClient;
import de.dfki.vsm.api.VSMScenePlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.model.scenescript.AbstractWord;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.runtime.values.StructValue;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * @author Not me
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
    private HCMScenePlayer() {


        // Print Some Debug Information
        mLogger.message("Creating HCM Scene Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static synchronized HCMScenePlayer getInstance() {
        if (sInstance == null) {
            sInstance = new HCMScenePlayer();
        }

        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final boolean launch(final RunTimeProject project) {

        // Load Parent Scene Player
        super.launch(project);

        // Initialize Scene Player
        try {

            // Initialize Event Handler
            final String lhost = mPlayerConfig.getProperty("hcm.handler.local.host");
            final String rhost = mPlayerConfig.getProperty("hcm.handler.remote.host");
            final String lport = mPlayerConfig.getProperty("hcm.handler.local.port");
            final String rport = mPlayerConfig.getProperty("hcm.handler.remote.port");
            final String rflag = mPlayerConfig.getProperty("hcm.handler.remote.flag");

            // Debug Some Information
            mLogger.message("" + "HCM Event Handler Local Host  : '" + lhost + "'" + "\r\n"
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
            mLogger.failure(exc.toString());

            // Debug Some Information
            mLogger.message("Failed Launching HCM Scene Player");
        }

        // Create User Interface
        mUserInterface = new HCMUserInterface(this);

        // Print Debug Information
        mLogger.message("Launching HCM Scene Player");
        
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final boolean unload() {

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
            mLogger.message("Joining HCM Event Handler");
        } catch (Exception exc) {

            // Print Debug Information
            mLogger.warning(exc.toString());
        }

        // Print Debug Information
        mLogger.message("Unloading HCM Scene Player");
        
        return true;
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
            mLogger.message("Agent #" + client.getAgentUaid() + " Receiving Data '" + input + "'");

            // Parse The Data To A New Message
            final HCMEventObject message = HCMEventObject.getInstance(input);

            // Find The Message Task Identifier
            if (message.getUtid() != null) {

                // Get The Message Task Identifier
                final String utid = message.getUtid();

                // Notify The Right Waiting Thread
                synchronized (mThreadQueue) {

                    // Remove Respective Task
                    final Thread task = mThreadQueue.remove(utid);

                    // Debug Some Information
                    mLogger.message("Removing The Waiting Thread '" + task + "' With Id '" + utid + "'");

                    // Debug Some Information
                    mLogger.message("Notifying All Waiting Threads");

                    // Notify Waiting Tasks
                    mThreadQueue.notifyAll();
                }
            } else {

                // Debug Some Information
                mLogger.warning("Message Has Wrong Format '" + input + "'");
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
        final HCMEventObject message = HCMEventObject.newInstance(type, utid, name, uaid, text, date, time);

        // Send The New Command
        synchronized (mThreadQueue) {
            if (client.sendString(message.toString())) {

                // Print Information
                mLogger.message("Enqueuing Waiting Thread '" + task + "' With Id '" + utid + "'");

                // Enqueue The Waiting Thread
                mThreadQueue.put(utid, Thread.currentThread());

                // And Await The Notification
                while (mThreadQueue.containsKey(utid)) {

                    // Print Information
                    mLogger.message("Awaiting Notification For '" + task + "' With Id '" + utid + "'");

                    // Print Information
                    try {
                        mThreadQueue.wait();
                    } catch (final InterruptedException exc) {

                        // Print Information
                        mLogger.message("Interrupting Waiting Task '" + task + "' With Id '" + utid + "'");
                    }

                    // Print Information
                    mLogger.message("Searching Notification For '" + task + "' With Id '" + utid + "'");
                }

                // Print Information
                mLogger.message("Received Notification For '" + task + "' With Id '" + utid + "'");
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
                final SceneScript sceneScript = mProject.getSceneScript();

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
                            final VSMAgentClient client = mAgentMap.get(name);

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
                                mLogger.failure("Agent '" + name + "'Does Not Exist");
                            }
                        } else {

                            // Get The Default Speaker
                            mLogger.failure("Turn Has No Speaker Definition");
                        }

                        // Exit If Interrupted After Utterance
                        if (isDone()) {

                            // Print Information
                            mLogger.message("Finishing Task '" + utid + "' After Utterance");

                            return;
                        }
                    }

                    // Exit If Interrupted After Turn
                    if (isDone()) {

                        // Print Information
                        mLogger.message("Finishing Task '" + utid + "' After Turn");

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
                mLogger.warning("Interrupting '" + Thread.currentThread().getName()
                        + "' During The Execution Of Scene '" + sceneName + "'");
                // Terminate The Task
                task.abort();
                // Interrupt The Task
                task.interrupt();
            }
        }
    }
}
