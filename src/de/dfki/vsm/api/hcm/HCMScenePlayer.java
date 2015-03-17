package de.dfki.vsm.api.hcm;

import de.dfki.vsm.api.VSMAgentClient;
import de.dfki.vsm.api.VSMScenePlayer;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.script.AbstractWord;
import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.model.script.SceneObject;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.script.SceneTurn;
import de.dfki.vsm.model.script.SceneUttr;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.StructValue;
import de.dfki.vsm.runtime.Process;
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
    private HCMEventHandler mHCMEventHandler;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private HCMScenePlayer(final ProjectData project) {
        // Initialize The Scene Player
        super(project);
        // Print Some Debug Information
        mVSM3Log.message("Creating HCM Scene Player");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static synchronized HCMScenePlayer getInstance(final ProjectData project) {
        if (sInstance == null) {
            sInstance = new HCMScenePlayer(project);
        }
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void launch() {
        // Load Basic Scene Player
        super.launch();
        //
        try {
            // Initialize The HCM Event Handler
            final String lhost = mPlayerConfig.property("hcm.handler.local.host");
            final String rhost = mPlayerConfig.property("hcm.handler.remote.host");
            final String lport = mPlayerConfig.property("hcm.handler.local.port");
            final String rport = mPlayerConfig.property("hcm.handler.remote.port");
            final String rflag = mPlayerConfig.property("hcm.handler.remote.flag");
            // Debug Some Information
            mVSM3Log.message(""
                    + "HCM Event Handler Local Host  : '" + lhost + "'" + "\r\n"
                    + "HCM Event Handler Local Port  : '" + lport + "'" + "\r\n"
                    + "HCM Event Handler Remote Host : '" + rhost + "'" + "\r\n"
                    + "HCM Event Handler Remote Port : '" + rport + "'" + "\r\n"
                    + "HCM Event Handler Remote Flag : '" + rflag + "'");
            // Construct The HCM Event Handler
            mHCMEventHandler = new HCMEventHandler(this);
            // Initialize The HCM Event Handler
            mHCMEventHandler.init(
                    lhost, Integer.parseInt(lport),
                    rhost, Integer.parseInt(rport),
                    Boolean.valueOf(rflag));
            // Start The HCM Event Handler Now
            mHCMEventHandler.start();
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
        // Unload The Scene Player
        super.unload();
        // Dispose User Interface
        mUserInterface.dispose();
        // Abort The HCM Socket
        mHCMEventHandler.abort();
        // Join With All Threads
        try {
            // Join With The HCM Socket
            mHCMEventHandler.join();
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
        final String data = client.recvString();
        // Check The Incoming Data String
        if (data != null) {
            // Debug Some Information
            mVSM3Log.message("VSM Agent Client Receiving Data '" + data + "'");
            // Parse The Data To A New Message
            final HCMEventMessage message = new HCMEventMessage(data);
            // Find The Message Task Identifier
            if (message.hasTask()) {
                // Get The Message Task Identifier
                final String threadId = message.getTask();
                // Notify The Right Waiting Thread
                synchronized (mWaitingThreadQueue) {
                    // Remove Respective Task 
                    final Thread threadObj = mWaitingThreadQueue.remove(threadId);
                    // Debug Some Information
                    mVSM3Log.message("Removing The Waiting Thread '" + threadObj + "' With Id '" + threadId + "'");
                    // Debug Some Information
                    mVSM3Log.message("Notifying All Waiting Threads");
                    // Notify Waiting Tasks
                    mWaitingThreadQueue.notifyAll();
                }
            } else {
                // Debug Some Information
                mVSM3Log.warning("Message Has Wrong Format '" + data + "'");
            }
        } else {
            // Debug Some Information
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void request(
            final String type,
            final String task,
            final String name,
            final String uaid,
            final String text,
            final long vsmTime,
            final long sysTime,
            final Thread threadObj,
            final VSMAgentClient client) {
        // Create The New Command 
        final String message = "<action "
                + "type=\"" + type + "\" "
                + "task=\"" + task + "\" "
                + "name=\"" + name + "\" "
                + "uaid=\"" + uaid + "\" "
                + "date=\"" + sysTime + "\" "
                + "time=\"" + vsmTime + "\">"
                + text
                + "</action>";
        // Send The New Command
        synchronized (mWaitingThreadQueue) {
            if (client.sendString(message)) {
                // Print Information
                mVSM3Log.message("Enqueuing Waiting Thread '" + threadObj + "' With Id '" + task + "'");
                // Enqueue The Waiting Thread
                mWaitingThreadQueue.put(task, Thread.currentThread());
                // And Await The Notification
                while (mWaitingThreadQueue.containsKey(task)) {
                    // Print Information
                    mVSM3Log.message("Awaiting Notification For '" + threadObj + "' With Id '" + task + "'");
                    // Print Information
                    try {
                        mWaitingThreadQueue.wait();
                    } catch (final InterruptedException exc) {
                        // Print Information
                        mVSM3Log.message("Interrupting Waiting Task '" + threadObj + "' With Id '" + task + "'");
                    }
                    // Print Information
                    mVSM3Log.message("Searching Notification For '" + threadObj + "' With Id '" + task + "'");
                }
                // Print Information
                mVSM3Log.message("Received Notification For '" + threadObj + "' With Id '" + task + "'");
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void play(final String sceneName, final LinkedList<AbstractValue> sceneArgs) {
        // Initialize Parameter Data
        final Process threadName = (Process) Thread.currentThread();
        final String taskId = threadName.getName() + "::" + sceneName;
        // And Process Scene Arguments
        final HashMap<String, String> argMap = new HashMap<>();
        if (sceneArgs != null && !sceneArgs.isEmpty()) {
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
        Task task = new Task(taskId) {
            @Override
            public void run() {
                // Select A Scene From Script
                final SceneScript sceneScript = mProjectData.getSceneScript();
                // Select Default Scene Group
                final SceneGroup sceneGroup = sceneScript.getSceneGroup(sceneName);
                // Select Language Dependent Group
                final SceneGroup langGroup = sceneScript.getSceneGroup("de", sceneName);
                // Select A Scene From Group
                final SceneObject selectedScene
                        = (langGroup != null) ? (langGroup.select()) : (sceneGroup.select());
                // Infer The Final Scene Language
                //final String selectedLang = selectedScene.getLanguage();
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
                            if (!sceneWord.equals(sceneUtt.getWordList().get(
                                    sceneUtt.getWordList().size() - 1))) {
                                textBuilder.append(' ');
                            }
                        }
                        // Check Action Builder Length
                        if (textBuilder.length() > 0) {
                            // Append Punctation MArk
                            textBuilder.append(sceneUtt.getPunct());
                        }
                        // Get The Speaker Name
                        final String agentName = sceneTurn.getSpeaker();
                        // Check if The Speaker Exists
                        if (agentName != null) {
                            // Get The Adequate Agent
                            final VSMAgentClient vsmClient = mAgentClientMap.get(agentName);
                            // Check if The Client Exists
                            if (vsmClient != null) {
                                // Get The Current Thread Object
                                final Thread threadObj = (Thread) this;
                                // Get The Current Player Time
                                final long vsmTime = getCurrentTime();
                                // Get The Agent Unique Id
                                final String agentUaid = vsmClient.getAgentUaid();
                                // Get The Current System Time
                                final long sysTime = System.currentTimeMillis();
                                // Get the Message Text Content 
                                final String vsmMessage = textBuilder.toString();
                                // Request Command Execution
                                request("utterance", taskId, agentName, agentUaid,
                                        vsmMessage, vsmTime, sysTime, threadObj, vsmClient);
                            } else {
                                // Get The Default Speaker
                                mVSM3Log.failure("Agent '" + agentName + "'Does Not Exist");
                            }
                        } else {
                            // Get The Default Speaker
                            mVSM3Log.failure("Turn Has No Speaker Definition");
                        }
                        // Exit If Interrupted After Utterance
                        if (mIsDone) {
                            // Print Information
                            mVSM3Log.message("Finishing Task '" + taskId + "' After Utterance");
                            return;
                        }
                    }
                    // Exit If Interrupted After Turn
                    if (mIsDone) {
                        // Print Information
                        mVSM3Log.message("Finishing Task '" + taskId + "' After Turn");
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
