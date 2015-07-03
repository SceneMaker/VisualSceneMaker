package de.dfki.vsm.runtime.player;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.event.SceneExecutedEvent;
import de.dfki.vsm.editor.event.TurnExecutedEvent;;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.script.SceneGroup;
import de.dfki.vsm.model.script.SceneObject;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.script.SceneTurn;
import de.dfki.vsm.runtime.Process;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.AbstractValue.Type;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.StructValue;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

import static java.lang.Thread.sleep;

/**
 * @author Gregor Mehlmann
 */
public class DefaultDialogueActPlayer implements Player {

    // The Logger Instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Player Properties
    // private final PlayerConfig mProperties;
    // The Current Project
    private final ProjectData mProject;

    // Construct A Default Player
    public DefaultDialogueActPlayer(final ProjectData project) {
        mProject = project;

        // mProperties = project.getScenePlayerProperties();
    }

    // Launch The Default Player
    @Override
    public final void launch() {}

    // Unload The Default Player
    @Override
    public final void unload() {}

    @Override
    public final void play(final String name, final LinkedList<AbstractValue> args) {
        final Process                 process        = ((Process) java.lang.Thread.currentThread());
        final HashMap<String, String> mSceneParamMap = new HashMap<String, String>();

        // Process The Arguments
        if ((args != null) &&!args.isEmpty()) {

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

        // Create The Player Task
        Task task = new Task(process.getName() + name) {
            @Override
            public void run() {

                // while (true) {
                //// Exit If Interrupted
                // if (mIsDone) {
                // return;
                // }
                // }
                final SceneScript script = mProject.getSceneScript();
                final SceneGroup  group  = script.getSceneGroup(name);
                final SceneObject scene  = group.select();

                // Scene Visualization
                mLogger.message("Executing dialogAct:\r\n" + scene.getText());
                EventCaster.getInstance().convey(new SceneExecutedEvent(this, scene));

                // Process The Turns
                for (SceneTurn turn : scene.getTurnList()) {

                    // Turn Visualization
                    mLogger.message("Executing turn:" + turn.getText());
                    EventCaster.getInstance().convey(new TurnExecutedEvent(this, turn));

                    // Get The Turn Speaker
                    // Count The Word Number
                    int wordCount = 0;

                    // Utterance Simulation
                    try {
                        sleep(wordCount * 100);
                    } catch (InterruptedException exc) {
                        mLogger.warning(exc.toString());
                    }

                    // Exit If Interrupted
                    if (mIsDone) {
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
            } catch (Exception exc) {

                // Abort The Player Task
                task.mIsDone = true;
            }
        }
    }
}
