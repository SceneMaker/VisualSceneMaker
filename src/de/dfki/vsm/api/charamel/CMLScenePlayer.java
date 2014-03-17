package de.dfki.vsm.api.charamel;

import de.dfki.vsm.api.VSMAgentClient;
import de.dfki.vsm.api.VSMScenePlayer;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.script.AbstractWord;
import de.dfki.vsm.model.script.ActionFeature;
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
import de.dfki.vsm.util.bin.BINUtilities;
import de.dfki.vsm.runtime.Process;
import de.dfki.vsm.runtime.player.ScenePlayer.Task;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class CMLScenePlayer extends VSMScenePlayer {

    // The Message Pattern
    private static final Pattern mPattern
            = Pattern.compile(""
                    + "<\\?xml version='1\\.0'\\?>\\n"
                    + "<cai_response>\\n"
                    + "<cai_command id='(.*?)'>"
                    + "RenderXML"
                    + "</cai_command>\\n"
                    + "\\t<status>(.*?)</status>\\n"
                    + "</cai_response>\\n");
    // The Scene Player
    public static CMLScenePlayer sInstance;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static synchronized CMLScenePlayer getInstance(final ProjectData project) {
        if (sInstance == null) {
            sInstance = new CMLScenePlayer(project);
        }
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private CMLScenePlayer(final ProjectData project) {
        super(project);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void play(final String name, final LinkedList<AbstractValue> args) {
        // Initialize Parameter Data
        final Process thread = ((Process) Thread.currentThread());
        final String taskid = thread.getName() + name;
        // And Process Scene Arguments
        final HashMap<String, String> map = new HashMap<>();
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
        Task task = new Task(taskid) {
            @Override
            public void run() {
                // Select A Scene
                final SceneScript script = mProject.getSceneScript();
                final SceneGroup group = script.getGroup(name);
                final SceneObject scene = group.select();
                // Process Turns
                for (SceneTurn turn : scene.getTurnList()) {
                    // Get The Speaker
                    final String speaker = turn.getSpeaker();
                    // Get The Agent Client
                    final VSMAgentClient client = getAgentClient(speaker);
                    // Get Unique Agent Id
                    final String uaid = client.getUaid();
                    // Process Utterances
                    for (SceneUttr utterance : turn.getUttrList()) {
                        // The Final Text Is Initially Empty
                        String text = new String();
                        // Process the words of this utterance
                        for (AbstractWord word : utterance.getWordList()) {
                            if (word instanceof SceneParam) {
                                // Append Param
                                text += map.get(((SceneParam) word).getName());
                            } else if (word instanceof ActionObject) {
                                ActionObject action = (ActionObject) word;
                                // Append Text 
                                //text += action.getText(map);
                                text += "$(" + action.getName();
                                // Append The Features
                                for (ActionFeature feature : action.getFeatureList()) {
                                    // Only Consider Path Features
                                    if (feature.getKey().equals("path")) {
                                        // Get The Path
                                        final String path = feature.getVal();
                                        // Trim The Path
                                        final String trim = path.substring(1, path.length() - 1);
                                        // Append The Path
                                        text += "," + trim;
                                    }
                                }
                                text += ")";

                            } else {
                                // Append Word 
                                text += word.getText();
                            }
                            // Append Whitespace
                            if (!word.equals(utterance.getWordList().get(
                                    utterance.getWordList().size() - 1))) {
                                text += " ";
                            }
                        }
                        // Append Punctation
                        text += utterance.getPunct();
                        // Construct Command
                        final String message = "\n"
                                + "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                                + "\t<cai_request version=\"1.0\">\n"
                                + "\t\t<cai_command id=\"" + (taskid) + "\" aid=\"" + uaid + "\">"
                                + "RenderXML\n"
                                + "\t\t\t<animation_track>\n"
                                + "\t\t\t\t<speak_text>" + text + "</speak_text>\n"
                                + "\t\t\t</animation_track>\n"
                                + "\t\t</cai_command>\n"
                                + "\t</cai_request>\n";

                        // Debug Some Information
                        mLogger.message("Encoding '" + message + "' On '" + speaker + "' With Id '" + uaid + "'");
                        // Send Message And Then
                        // Await The Notification
                        synchronized (mPlayerTaskQueue) {
                            // Log The Notification
                            //nova("send", message);
                            final byte[] bytes = message.getBytes(/*"UTF-8"*/);
                            // Send Message And Then
                            if (client.send(BINUtilities.IntToBytesLE(100))
                                    && client.send(BINUtilities.IntToBytesLE(0))
                                    && client.send(BINUtilities.IntToBytesLE(bytes.length))
                                    && client.send(bytes)) {
                                // Log Some Nova Message
                                // TODO: Make CONTINUED
                                nova("send_action", message);
                                // Add Task To Waiters
                                mPlayerTaskQueue.put(taskid, this);
                            }
                            // While The Task Waits
                            while (mPlayerTaskQueue.containsKey(taskid)
                                    && mPlayerTaskQueue.containsValue(this)) {
                                try {
                                    // Print Information
                                    mLogger.message("Enqueuing Task '" + taskid + "'");
                                    // Wait For Notify
                                    mPlayerTaskQueue.wait();
                                    // Print Information
                                    mLogger.message("Notifying Task '" + taskid + "'");
                                } catch (Exception exc) {
                                    // Print Information
                                    mLogger.message("Interrupting Task '" + taskid + "'");
                                    // Exit If Interrupted
                                    if (mIsDone) {
                                        // Print Information
                                        mLogger.message("Finishing Task '" + taskid + "'");
                                        return;
                                    }
                                }
                            }
                        }

                        // Exit If Interrupted
                        if (mIsDone) {
                            // Print Information
                            mLogger.message("Finishing Task '" + taskid + "'");
                            return;
                        }
                    }
                    // Exit If Interrupted
                    if (mIsDone) {
                        // Print Information
                        mLogger.message("Finishing Task '" + taskid + "'");
                        return;
                    }
                }
                // Print Information
                mLogger.message("Finishing Task '" + taskid + "'");
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
                mLogger.warning("Interrupting '" + thread.getName() + "' In Scene '" + name + "'");
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
        // Receive The Header
        final byte[] header = client.recv(12);
        // Inspect The Header
        final byte[] msgtag = Arrays.copyOfRange(header, 0, 4);
        final byte[] status = Arrays.copyOfRange(header, 4, 8);
        final byte[] length = Arrays.copyOfRange(header, 8, 12);
        // Compute The Length
        final int size = BINUtilities.BytesLEToInt(length);
        // Receive The Message                   
        final byte[] ackn = client.recv(size);
        // Convert The Message
        final String data = new String(ackn);
        // Debug Some Information
        mLogger.message("Agent Client Receiving Notification '" + data + "'");
        // Define Ack Pattern
        final Matcher matcher = mPattern.matcher(data);
        // Find Some Matches
        if (matcher.matches()) {
            // Compute The Action Parameters
            final String action = matcher.group(0);
            final String uident = matcher.group(1);
            final String result = matcher.group(2);
            // Get The Waiters Lock
            synchronized (mPlayerTaskQueue) {
                // Print Information
                mLogger.message("Removing Task '" + uident + "' With Status '" + result + "'");
                // Remove The Task 
                mPlayerTaskQueue.remove(uident);
                // Notify Waitings
                mPlayerTaskQueue.notifyAll();
            }
            // Log Some Nova Message
             // TODO: Make COMPLETED
            nova("recv_action", action);
        }
    }
}
