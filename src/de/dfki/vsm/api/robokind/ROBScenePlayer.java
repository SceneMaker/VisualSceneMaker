package de.dfki.vsm.api.robokind;

import de.dfki.vsm.api.*;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.script.AbstractWord;
import de.dfki.vsm.model.script.ActionObject;
import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.model.script.SceneObject;
import de.dfki.vsm.model.script.SceneParam;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.script.SceneTurn;
import de.dfki.vsm.model.script.SceneUttr;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.StructValue;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class ROBScenePlayer extends VSMScenePlayer {

    // The Message Pattern
    private final Pattern mPattern = Pattern.compile(""
            + "<action "
            + "name=\"(.*?)\" "
            + "type=\"(.*?)\" "
            + "task=\"(.*?)\" "
            + "date=\"(.*?)\" "
            + "time=\"(.*?)\">"
            + "(.*?)"
            + "</action>");
    // The Scene Player
    public static ROBScenePlayer sInstance;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static synchronized ROBScenePlayer getInstance(final ProjectData project) {
        if (sInstance == null) {
            sInstance = new ROBScenePlayer(project);
        }
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ROBScenePlayer(final ProjectData project) {
        super(project);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void print(
            final String name, final String cond, final String text) {
        // Log Some Message With Logger
        final File dir = new File("./log/" + name);
        // Create A Directory For User
        if (!dir.exists()) {
            //
            mVSM3Log.message("Creating Evaluation Directory '" + dir + "'");
            // Create A Directory For User
            dir.mkdir();
        }
        try {
            // Log Some Message With Logger
            final File file = new File("./log/" + name + "/" + cond + ".txt");
            // Create A New Condition File 
            if (!file.exists()) {
                //
                mVSM3Log.message("Creating Evaluation Data File '" + file + "'");
                file.createNewFile();
            }

            //
            mVSM3Log.message("Logging Evaluation Data To File '" + file + "'");
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
            exc.printStackTrace();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void play(final String name, final LinkedList<AbstractValue> args) {
        // Initialize Parameter Data
        // And Process Scene Arguments
        final de.dfki.vsm.runtime.Process thread = ((de.dfki.vsm.runtime.Process) Thread.currentThread());
        final String ident = thread.getName().toUpperCase() + name;
        final HashMap<String, String> map = new HashMap<String, String>();
        if (args != null && !args.isEmpty()) {
            final AbstractValue value = args.getFirst();
            if (value.getType().equals(AbstractValue.Type.STRUCT)) {
                final StructValue struct = ((StructValue) value);
                for (final Map.Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
                    if (entry.getValue().getType() == AbstractValue.Type.STRING) {
                        final StringValue string = (StringValue) entry.getValue();
                        map.put(entry.getKey(), string.getValue());
                    }
                }
            }
        }

        // Execute The Player Task
        Task task = new Task(ident) {
            @Override
            public void run() {
                // Select A Scene
                final SceneScript script = mProjectData.getSceneScript();
                final SceneGroup group = script.getSceneGroup(name);
                final SceneObject scene = group.select();
                // Process Turns
                for (SceneTurn turn : scene.getTurnList()) {
                    // Get The Speaker
                    final String speaker = turn.getSpeaker();
                    // Process Utterances
                    for (SceneUttr utt : turn.getUttrList()) {
                        // The Final Text Is Initially Empty
                        String text = new String();
                        // Process the words of this utterance
                        for (AbstractWord word : utt.getWordList()) {
                            if (word instanceof SceneParam) {
                                // Append Param
                                text += map.get(((SceneParam) word).getName());
                            } else if (word instanceof ActionObject) {
                                ActionObject action = (ActionObject) word;
                                // Append Text 
                                text += action.getText(map);
                            } else {
                                // Append Word 
                                text += word.getText();
                            }
                            // Append Whitespace
                            if (!word.equals(utt.getWordList().get(utt.getWordList().size() - 1))) {
                                text += " ";
                            }
                        }
                        // Append Punctation
                        text += utt.getPunct();
                        // Create The Command 
                        final String message = ""
                                + "<action "
                                + "name=\"" + speaker + "\" "
                                + "type=\"" + "utterance" + "\" "
                                + "task=\"" + ident + "\" "
                                + "date=\"" + System.currentTimeMillis() + "\" "
                                + "time=\"" + getCurrentTime() + "\">"
                                + text
                                + "</action>";
                        // Debug Some Information
                        mVSM3Log.message("Encoding '" + message + "' On '" + speaker + "'");
                        // Send Message And Then
                        // Await The Notification
                        synchronized (mWaitingThreadQueue) {
                            // Log The Notification
                            //nova("send", message);
                            // Send Message And Then
                            getAgentClient(speaker).sendString(message);
                            // Add Task To Waiters
                            mWaitingThreadQueue.put(ident, this);
                            // While The Task Waits
                            while (mWaitingThreadQueue.containsKey(ident)
                                    && mWaitingThreadQueue.containsValue(this)) {
                                try {
                                    // Print Information
                                    mVSM3Log.message("Enqueuing Task '" + ident + "'");
                                    // Wait For Notify
                                    mWaitingThreadQueue.wait();
                                    // Print Information
                                    mVSM3Log.message("Notifying Task '" + ident + "'");
                                } catch (Exception exc) {
                                    // Print Information
                                    mVSM3Log.message("Interrupting Task '" + ident + "'");
                                    // Exit If Interrupted
                                    if (mIsDone) {
                                        // Print Information
                                        mVSM3Log.message("Finishing Task '" + ident + "'");
                                        return;
                                    }
                                }
                            }
                        }
                        // Log The Notification
                        //nova("recv", message);
                        // Exit If Interrupted
                        if (mIsDone) {
                            // Print Information
                            mVSM3Log.message("Finishing Task '" + ident + "'");
                            return;
                        }
                    }
                    // Exit If Interrupted
                    if (mIsDone) {
                        // Print Information
                        mVSM3Log.message("Finishing Task '" + ident + "'");
                        return;
                    }
                }
                // Print Information
                mVSM3Log.message("Finishing Task '" + ident + "'");
            }
        };
        // Start Player Task
        task.start();
        // Wait For The Task
        boolean finished = false;
        while (!finished) {
            try {
                // Join The Player Task
                task.join();
                // Set Termination Flag
                finished = true;
            } catch (Exception exc) {
                // Print Information
                mVSM3Log.warning("Interrupting '" + thread.getName() + "' In Scene '" + name + "'");
                // Terminate The Task
                task.mIsDone = true;
                // Interrupt The Task
                task.interrupt();
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected final void handle(final VSMAgentClient client) {
        // Receive A Message
        final String msg = client.recvString();
        // Check The Message
        if (msg != null) {
            // Define Ack Pattern
            final Matcher matcher = mPattern.matcher(msg);
            // Debug Some Information
            mVSM3Log.message("Agent Client Receiving Notification '" + msg + "'");
            // Find Some Matches
            if (matcher.matches()) {
                // Compute The Action Parameters
                final String action = matcher.group(0);
                final String name = matcher.group(1);
                final String type = matcher.group(2);
                final String task = matcher.group(3);
                final String date = matcher.group(4);
                final String time = matcher.group(5);
                final String text = matcher.group(6);
                // Get The Waiters Lock
                synchronized (mWaitingThreadQueue) {
                    // Print Information
                    mVSM3Log.message("Removing Task '" + task + "'");
                    // Remove The Task 
                    mWaitingThreadQueue.remove(task);
                    // Notify Waitings
                    mWaitingThreadQueue.notifyAll();
                }
            }
        } else {
            // Socket Closed Or Corrupted
            client.abort();
        }
    }
}
