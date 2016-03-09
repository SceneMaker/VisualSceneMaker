package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.UtteranceElement;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.player.activity.ActionActivity;
import de.dfki.vsm.runtime.player.activity.VerbalActivity;
import de.dfki.vsm.runtime.player.executor.AbstractExecutor;
import de.dfki.vsm.runtime.player.scheduler.ActivityScheduler;
import static de.dfki.vsm.runtime.player.scheduler.AbstractScheduler.SchedulingPolicy.BLOCKING;
import static de.dfki.vsm.runtime.player.scheduler.AbstractScheduler.SchedulingPolicy.CONCURRENT;
import de.dfki.vsm.runtime.player.trigger.MarkTrigger;
import de.dfki.vsm.runtime.player.trigger.TimeTrigger;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.charamel.Charamel;
import de.dfki.vsm.xtension.stickman.Stickman;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.concurrent.TimeUnit;

/**
 * @author Gregor Mehlmann
 */
public final class ScenePlayer implements AbstractPlayer {

    // The static marker id
    private static Long sId = 0x0L;

    // Get unique marker id
    private static Long newId() {
        return ++sId;
    }

    // The runtime environment
    private final RunTimeInstance mRunTime
            = RunTimeInstance.getInstance();
    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The runtime project data
    private RunTimeProject mProject;

    private final HashMap<String, AbstractExecutor> mExecutorMap = new HashMap();

    private final ActivityScheduler mScheduler = new ActivityScheduler();

    public ScenePlayer(final RunTimeProject project) {
        mProject = project;
    }

    @Override
    public boolean launch() {
        mLogger.message("Launching scene player");
        // Load the executors now
        // Do that from the config!
        mExecutorMap.put("sticky", new Stickman());
        mExecutorMap.put("gloria", new Charamel());
        // Launch the scheduler
        return mScheduler.launch();

    }

    @Override
    public boolean unload() {
        mLogger.message("Unloading scene player");
        // Unload the schdeuler
        return mScheduler.unload();
    }

    @Override
    public boolean play(final String name, final LinkedList args) {
        // Get the executing process
        final Process process = (Process) Thread.currentThread();
        //
        mLogger.message("Playing scene '" + name + "' in process '" + process + "'");
        // Translate the argument list
        final HashMap map = new HashMap();
        // Get the scene language group 
        final SceneScript script = mProject.getSceneScript();
        final SceneGroup group = script.getSceneGroup("en", name);
        // Select a new scene
        final SceneObject scene = group.select();
        // Process the scene
        for (SceneTurn turn : scene.getTurnList()) {
            // Get the executor for this turn
            final AbstractExecutor turnExecutor = mExecutorMap.get(turn.getSpeaker());
            //
            for (SceneUttr uttr : turn.getUttrList()) {
                //
                final LinkedList<String> builder = new LinkedList();
                //
                for (final UtteranceElement element : uttr.getWordList()) {
                    if (element instanceof ActionObject) {
                        final ActionObject action = (ActionObject) element;
                        // Get the actor name of this action
                        final String actor = action.getActorName();
                        // Get the executor for this action
                        final AbstractExecutor actionExecutor
                                = (actor != null ? mExecutorMap.get(actor) : turnExecutor);
                        // Create a new marker for the action
                        final String marker = actionExecutor.getMarker(newId());
                        // Append the marker to the activity
                        builder.add(marker);
                        // Register the activity with marker
                        mScheduler.register(
                                CONCURRENT,
                                new MarkTrigger(marker),
                                new ActionActivity(action.getText(map)),
                                actionExecutor);
                    } else {
                        // Append the text to the activity
                        builder.add(element.getText(map));
                    }
                }
                // 
                final String mark = uttr.getPunctuationMark();
                //
                mScheduler.register(
                        BLOCKING,
                        new TimeTrigger(0, TimeUnit.MILLISECONDS),
                        new VerbalActivity(builder, mark),
                        turnExecutor);
            }
        }
        return true;
    }
}
