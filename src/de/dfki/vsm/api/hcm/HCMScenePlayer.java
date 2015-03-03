package de.dfki.vsm.api.hcm;



import de.dfki.vsm.api.VSMAgentClient;
import de.dfki.vsm.api.VSMScenePlayer;

import de.dfki.vsm.editor.event.SceneExecutedEvent;
import de.dfki.vsm.editor.event.TurnExecutedEvent;
import de.dfki.vsm.editor.event.UtteranceExecutedEvent;

import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.script.AbstractWord;
import de.dfki.vsm.model.script.ActionObject;
import de.dfki.vsm.model.script.SceneAbbrev;
import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.model.script.SceneObject;
import de.dfki.vsm.model.script.SceneParam;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.script.SceneTurn;
import de.dfki.vsm.model.script.SceneUttr;
import de.dfki.vsm.model.script.SceneWord;

import de.dfki.vsm.runtime.Process;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.StructValue;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.AbstractValue.Type;

import de.dfki.vsm.util.evt.EventCaster;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import static java.lang.Thread.sleep;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

//TODO:
// - avoid freezing the entire scene flow when one of the proxies crashes
//   or drops out of the network
//   - approach: configurable time out for answer?
//   - possible recovery strategies?
//      - switch to simulation mode until proxy is online again?
//      - mark as (un)available in the fact base or a scene flow variable?
//          => remaining agents could use that to adapt their turn management
// - convert non-String SceneParams 


/**
 * A VSMScenePlayer that connects to a remote VSM proxy application.
 * 
 * Enables VSM scenarios with a heterogenous agent setup,
 * such as different robot types and/or virtual agents
 * that might be programmed in different languages and
 * located on separate machines.
 * 
 * The player connects to a proxy application via network.
 * All these proxies must be able to process the same data format.
 * 
 * @author Gregor Mehlmann, Kathrin Janowski
 */
public class HCMScenePlayer extends VSMScenePlayer {
    
    // the player instance
    public static HCMScenePlayer sInstance;
    //public static HCMScenePlayer sInstance;
    
    // the event handler (hides original SSI Event handler)
    protected SSIEventHandler mEventHandler;
    
    
    
    //==========================================================================
    // construction, launching and unloading
    //==========================================================================
    
    public static synchronized HCMScenePlayer getInstance(final ProjectData project) {
        if (sInstance == null) {
            sInstance = new HCMScenePlayer(project);
        }

        return sInstance;
    }

    // construct the player
    private HCMScenePlayer(final ProjectData project) {
        super(project);  
    }

    // Launch The Default Player
    @Override
    public final void launch() {
        mVSM3Log.message("Launching HCM Remote Scene Player");
        mVSM3Log.message("HCM Remote Scene Player instance: "+sInstance.toString());
    
        //check directory
        /*try {
            File swiBase = new File(mPlayerConfig.property("vsm.swilbase"));
            mVSM3Log.message("SWI Base directory: "+swiBase.getCanonicalPath());
        } catch (Exception ex) {
            mVSM3Log.failure("Could not resolve SWI Base directory: "+ex.getMessage());
        }*/
        
        super.launch();
        
        //----------------------------------------------------------------------
        // start the SSI event handler
        //----------------------------------------------------------------------
        mEventHandler = new SSIEventHandler(this);
        
        // Initialize The Properties
        final String ssilhost = mPlayerConfig.property("vsm.ssilhost");
        final String ssilport = mPlayerConfig.property("vsm.ssilport");
        final String ssirhost = mPlayerConfig.property("vsm.ssirhost");
        final String ssirport = mPlayerConfig.property("vsm.ssirport");
        final String ssirconn = mPlayerConfig.property("vsm.ssirconn");
        
        // Print Out The Properties
        mVSM3Log.message(""
                + "SSI-Local-Host :" + ssilhost + "\r\n"
                + "SSI-Remote-Host :" + ssirhost + "\r\n"
                + "SSI-Local-Port :" + ssilport + "\r\n"
                + "SSI-Remote-Port :" + ssirport + "\r\n"
                + "SSI-Remote-Flag :" + ssirconn + "\r");
        
        // Initialize the handler
        try{
            int localPort = Integer.parseInt(ssilport);
            int remotePort = Integer.parseInt(ssirport);
            boolean remoteFlag = Boolean.parseBoolean(ssirconn);
            
            mEventHandler.init(ssilhost, localPort,
                    ssirhost, remotePort, remoteFlag);
            mEventHandler.start();
            mVSM3Log.message("SSI Event Handler ready");
        }
        catch(NumberFormatException nfe)
        {
            mVSM3Log.failure("Could not create SSI event handler: illegal value for port number(s)");
        }
    }

    // Unload The Default Player
    @Override
    public final void unload() {
        super.unload();
        
        mEventHandler.abort();
        try{
            mEventHandler.join();
            mVSM3Log.message("Event Handler stopped");
        }
        catch(InterruptedException ie)
        {
            mVSM3Log.warning("Interrupted while waiting for the event handler");
        }
        
        mVSM3Log.message("HCMRemoteScenePlayer unloaded");
    }

    
    //==========================================================================
    // executing the scene
    //==========================================================================
    
    /**
     * Executes a scene.
     * 
     * @param sceneName the name of the scene
     * @param args the arguments for this scene
     */
    @Override
    public final void play(final String sceneName, final LinkedList<AbstractValue> args) {
        // get the current thread
        final Process process = ((Process) java.lang.Thread.currentThread());
        String taskID = process.getName() + sceneName;
        
        // Process The Arguments -----------------------------------------------
        final HashMap<String, String> mSceneParamMap = new HashMap<String, String>();
        if (args != null && !args.isEmpty()) {
            // Get The First Argument
            final AbstractValue value = args.getFirst();
            // Check The Argument Type
            if (value.getType().equals(Type.STRUCT)) {
                // Cast The Argument Type
                final StructValue struct = (StructValue) value;
                // Process Scene Arguments
                for (final Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
                    if (entry.getValue().getType() == Type.STRING) {
                        mSceneParamMap.put(entry.getKey(), ((StringValue) entry.getValue()).getValue());
                    } else {
                        // Process Other Argument Types
                    }
                }
            }
        }

        // Create The Player Task ----------------------------------------------
        Task task = new Task(taskID) {
            @Override
            public void run() {
                // Select The Scene
                final SceneScript script = mProjectData.getSceneScript();
                final SceneGroup group = script.getSceneGroup(sceneName);
                final SceneObject scene = group.select();
                // Scene Visualization
                mVSM3Log.message("Executing scene:\r\n" + scene.getText());
                EventCaster.getInstance().convey(new SceneExecutedEvent(this, scene));
                
                // Process The Turns -------------------------------------------
                for (SceneTurn turn : scene.getTurnList()) {
                    // Turn Visualization
                    mVSM3Log.message("Executing turn:" + turn.getText());
                    EventCaster.getInstance().convey(new TurnExecutedEvent(this, turn));
                                        
                    // Count The Word Number
                    int wordCount = 0;
                    // Process Utterance
                    for (SceneUttr utt : turn.getUttrList()) {
                        //start assembling the action message
                        final StringBuilder actionBuilder = new StringBuilder();                        
                        // Utterance Visualization
                        mVSM3Log.message("Executing utterance:" + utt.getText());
                        EventCaster.getInstance().convey(new UtteranceExecutedEvent(this, utt));

                        //----------------------------------------------------------------------------
                        // Process the words of this utterance
                        //----------------------------------------------------------------------------
                        for (AbstractWord word : utt.getWordList()) {
                            // spoken text -----------------------------------------------------------
                            if (word instanceof SceneWord) {
                                // Visualization
                                mVSM3Log.message("Executing vocable:" + ((SceneWord) word).getText());
                                wordCount = ((SceneWord) word).getText().length();
                                // add to the action message 
                                actionBuilder.append(word.getText());
                            }
                            // scene parameter -------------------------------------------------------
                            else if (word instanceof SceneParam) {
                                // Visualization
                                mVSM3Log.message("Executing param:" + ((SceneParam) word).getText());
                                actionBuilder.append(mSceneParamMap.get(((SceneParam) word).getName()));
                            }
                            // embedded action -------------------------------------------------------
                            else if (word instanceof ActionObject) {
                                // Visualization
                                mVSM3Log.message("Executing action:" + ((ActionObject) word).getText());
                                // Get The Action Object
                                final ActionObject action = ((ActionObject) word);
                                // Continue With The Next Word
                                actionBuilder.append(action.getText(mSceneParamMap));
                            }
                            // abbreviation ----------------------------------------------------------
                            else if (word instanceof SceneAbbrev) {
                                // Visualization
                                mVSM3Log.message("Executing abbreviation:" + ((SceneAbbrev) word).getText());
                                // add to the action message 
                                actionBuilder.append(word.getText());
                            }
                            
                            // append whitespace -----------------------------------------------------
                            if (!word.equals(utt.getWordList().get(utt.getWordList().size() - 1))) {
                                actionBuilder.append(' ');
                            }
                        }
                        
                        //append punctuation mark ---------------------------------------
                        if (actionBuilder.length() > 0) {
                            actionBuilder.append(utt.getPunct());
                        }
                        
                        //------------------------------------------------------------------------------------
                        // execute the utterance
                        //------------------------------------------------------------------------------------
                        
                        // get the turn speaker --------------------------------
                        final String speaker = turn.getSpeaker();
                        VSMAgentClient client=null;
                        String agentUaid = "";
                        if (speaker != null)
                        {
                            client = mAgentClientMap.get(speaker);
                            agentUaid = client.getAgentUaid();
                        }
                        else{
                            // Get The Default Speaker
                            mVSM3Log.warning("turn has no speaker :"+turn.toString());
                        }
                    
                        // execute the turn ------------------------------------
                        if(client != null)
                        {
                            // Send request command to the receiver.
                            request(speaker, "utterance", this, System.nanoTime(),
                                    actionBuilder.toString(), client);
                        }
                        else
                        {
                            mVSM3Log.message("speaker not found -> simulating turn execution");
                            //simulate execution
                            try {
                                sleep(wordCount * 100);
                            } catch (InterruptedException exc) {
                                mVSM3Log.warning("interrupted while simulating turn execution");
                            }
                        }

                        // Exit if interrupted after utterance.
                        if (mIsDone) {
                            mVSM3Log.message("Finishing task '" + taskID + "' after utterance");
                            return;
                        }
                    }

                    // Exit if interrupted after turn.
                    if (mIsDone) {
                        // Print Information
                        mVSM3Log.message("Finishing Task '" + taskID + "' After Turn");
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
            } catch (Exception e) {
                // Print Information
                mVSM3Log.warning("Interrupting '" + Thread.currentThread().getName() + "' In Scene '" + sceneName + "'");
                // Terminate The Task
                task.mIsDone = true;
                // Interrupt The Task
                task.interrupt();
            }
        }
    }

    /**
     * Executes an animation directly.
     * 
     * @param agentName
     * @param animName 
     */
    public boolean anim(final String agentName, final String animName) {
        // get the current thread
        final Process process = ((Process) java.lang.Thread.currentThread());
        String taskID = process.getName() + animName;

        // Create The Player Task ----------------------------------------------
        Task task = new Task(taskID) {
            @Override
            public void run() {
                // Visualization
                mVSM3Log.message("Executing animation:\r\n" + animName);
                //TODO: what kind of event should be raised?
                //EventCaster.getInstance().convey(new SceneExecutedEvent(this, scene));

                //------------------------------------------------------------------------------------
                // execute the animation
                //------------------------------------------------------------------------------------
                // get the agent --------------------------------
                VSMAgentClient client = null;
                String agentUaid = "";
                if (agentName != null) {
                    client = mAgentClientMap.get(agentName);
                    agentUaid = client.getAgentUaid();
                } else {
                    mVSM3Log.warning("animation has no agent");
                }

                // execute the animation ------------------------------------
                if (client != null) {
                    // Send request command to the receiver.
                    request(agentName, "animation", this, System.nanoTime(),
                            animName, client);
                } else {
                    mVSM3Log.message("agent not found -> simulating animation execution");
                    //simulate execution
                    try {
                        sleep(100);
                    } catch (InterruptedException exc) {
                        mVSM3Log.warning("interrupted while simulating turn execution");
                    }
                }

                // Exit if interrupted after turn.
                if (mIsDone) {
                    // Print Information
                    mVSM3Log.message("Finishing Task '" + taskID + "' After Turn");
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
            } catch (Exception e) {
                // Print Information
                mVSM3Log.warning("Interrupting '" + Thread.currentThread().getName() + "' In Animation '" + animName + "'");
                // Terminate The Task
                task.mIsDone = true;
                // Interrupt The Task
                task.interrupt();
            }
        }
        
        return true;
    }
        
        
    //==========================================================================
    // messaging
    //==========================================================================
    
    /**
     * Sends the command to the proxy.
     * 
     * @param name the agent's name
     * @param type the action type
     * @param task the task to schedule
     * @param time the message's time
     * @param text the message's content
     * @param client the socket client for this agent
     */
    public final void request(String name, String type, Task task, long time, String text, VSMAgentClient client) {
        try {
            String taskID = task.getName();
            
            // Create The Command 
            final String message = "<action "
                + "name=\"" + name + "\" "
                + "type=\"" + type + "\" "
                + "task=\"" + taskID + "\" "
                + "date=\"" + System.currentTimeMillis() + "\" "
                + "time=\"" + getCurrentTime() + "\">"
                + text
                + "</action>";
            
                        
            synchronized (mWaitingThreadQueue) {
                if(client.sendString(message))
                {
                    mVSM3Log.message("Enqueuing waiting task '" + task + "'");
                    mWaitingThreadQueue.put(taskID, task);

                    while (mWaitingThreadQueue.containsKey(taskID)) {
                        mVSM3Log.message("Awaiting notification for '" + task + "'");
                        mWaitingThreadQueue.wait();
                        mVSM3Log.message("Searching notification for '" + task + "'");
                        mVSM3Log.message("Interrupting waiting task '" + task + "'");
                    }

                    mVSM3Log.message("Received Notification For '" + task + "'");
                }
            }
        } catch (Exception ex) {
            mVSM3Log.failure(ex.getMessage());
        }
    }

    
    
    /**
     * Handles the notification from the proxy.
     * @param client 
     */
    @Override
    public final void handle(VSMAgentClient client) {
        try {
            mVSM3Log.message("Awaiting notification from Proxy.");

            final String message = client.recvString();

            mVSM3Log.message("Received notification: " + message + "");

            
            if (message != null)
            {
                // Debug Some Information
                mVSM3Log.message("Agent Client Receiving Notification '" + message + "'");

                //------------------------------------------------------------------
                // Parse the message
                //------------------------------------------------------------------
                VSMMessage parsedMessage = new VSMMessage(message);
                if(parsedMessage.hasTask())
                {
                    String task = parsedMessage.getTask();
            
                    synchronized (mWaitingThreadQueue) {
                        mVSM3Log.message("Removing the waiting Task '" + task + "'");
                        mWaitingThreadQueue.remove(task);
                        mVSM3Log.message("Notifying All Waiting Tasks");
                        mWaitingThreadQueue.notifyAll();
                    }
                }
            }
        }
        catch (Exception ex) {
            mVSM3Log.failure(ex.getMessage());
        }
    }

    
    
    
    
    //==========================================================================
    // logging
    //==========================================================================
    
    /**
     * Write the given data to a log file.
     * 
     * @param subDir the sub-directory in "./log/
     * @param fileName name of the file, without extension
     * @param text the text content of the file
     */
    public final synchronized void writeToFile(
            final String subDir, final String fileName, final String text) {
        
        // Log Some Message With Logger
        final File dir = new File("./log/" + subDir);
        // Create A Directory For User
        if (!dir.exists()) {
            //
            mVSM3Log.message("Creating directory '" + dir + "'");
            // Create A Directory For User
            dir.mkdir();
        }
        try {
            // Log Some Message With Logger
            final File file = new File("./log/" + subDir + "/" + fileName + ".txt");
            // Create A New Condition File 
            if (!file.exists()) {
                //
                mVSM3Log.message("Creating file '" + file + "'");
                file.createNewFile();
            }

            //
            mVSM3Log.message("Writing data to file '" + file + "'");
            // Create The File Writer
            final FileWriter writer = new FileWriter(file);
            final BufferedWriter buffer = new BufferedWriter(writer);
            // Write The Line Of Data
            buffer.write(text);
            buffer.newLine();
            buffer.flush();
            // Close The Streams Finally
            buffer.close();
            writer.close();

        } catch (IOException exc) {
            mVSM3Log.failure("Could not write to file \""+fileName
                    +"\" in \""+dir.getPath()+"\": "
                    +exc.toString());
        }
    }
}
