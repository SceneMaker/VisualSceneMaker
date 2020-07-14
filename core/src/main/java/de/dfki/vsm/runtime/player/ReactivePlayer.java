package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.scenescript.*;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.interpreter.error.SceneDoesNotExists;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.jpl.JPLEngine;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public final class ReactivePlayer extends RunTimePlayer {

    // The static marker id
    private static long sId = 0x0L;

    // Get unique marker id
    private synchronized Long newId() {
        return sId++;
    }

    // Flag if we use the JPL
    private boolean mUseJPL = false;
    // The runtime timer
    //private RunTimeTimer mTimer = null;

    // Create the scene player
    public ReactivePlayer(
            final PlayerConfig config,
            final RunTimeProject project) {
        // Initialize the player
        super(config, project);
        // Print some information
        //mLogger.message("Creating reactive player '" + this + "' for project '" + project + "'");
        // Get the JPL flag value
        mUseJPL = Boolean.parseBoolean(mConfig.getProperty("usejpl"));
    }

    // Launch the player
    @Override
    public final void launch() {
        // Print some information
        mLogger.message("Launching reactive player '" + this + "'");
        // Load the fact base
        if (mUseJPL) {
            JPLEngine.load("swi/logic.pl");
        }
    }

    // Unload the player
    @Override
    public final void unload() {
        // Print some information
        mLogger.message("Unloading reactive player '" + this + "'");
    }

    // Call the play action activity method
    @Override
    public final void playAction(final String text, final List<AbstractValue> args) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Make unique worker name
        final String task = process.getName() + ":" + text + "@";
        // Translate the arguments
        final HashMap substitutions = getSubstitutions(args);
        // Print some information
        //mLogger.message("Playing Action '" + text + "' in process '" + process + "' on reactive player '" + this + "' with substitutions '" + substitutions.toString() + "'");
        // Create playback task
        final PlayerWorker worker;
        worker = new PlayerWorker(task) {
            @Override
            public void run() {
                // Parsing command string
                // TODO: Better use our parser for that!!!
                final String syntax = text.trim();
                String actor = "";
                String name = "";
                final LinkedList<ActionFeature> features = new LinkedList<>();

                int cnt = 0;

                if (syntax.startsWith("[") && syntax.endsWith("]")) {
                    // PG: changed action and action feature parser to be more powerful
                    // matching something like: agent Action x=2.5 y=0.0 z=-13.0 text='Someone wants a beer!' other=bad some='things' state='Da=fuck.continued and others']
                    final Pattern pattern = Pattern.compile("^\\w+|\\w+\\s|\\w+]|[a-zA-Z-_]+=[a-zA-Z]{1}[a-zA-Z-_]+|\\w+\\=-?[0-9\\.]+|\\w+='[\\wäöüßÄÖÜ\\s:\\.,!?=@/]+'");
                    final Matcher matcher = pattern.matcher(syntax);
                    while (matcher.find()) {
                        final String token = matcher.group().trim();
                        if (cnt == 0) {
                            actor = token;
                        } else if (cnt == 1) {
                            name = token;
                            name = (name.contains("]")) ? name.replace("]", "") : name;
                        } else if (token.contains("=")) {
                            String[] pair = token.split("=");
                            features.add(new ActionFeature(0, pair[0].length(), pair[0], pair[1]));
                        }
                        cnt++;
                    }
                }
                // Schedule the activity without delay but blocking
                final ActionActivity activity = new ActionActivity(actor, name, text, features, substitutions);
                activity.setType(AbstractActivity.Type.blocking);
                mScheduler.schedule(0, null, activity, mProject.getAgentDevice(actor));
            }
        };
        // Start the playback task
        worker.start();
        // Wait for playback task
        boolean finished = false;
        while (!finished) {
            try {
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                //mLogger.message("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                //mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }
        // Print some information
        //mLogger.message("Continuing '" + process + "'");
    }

    // Call the play scene group method
    @Override

    public final void playScene(final String name, final List<AbstractValue> args) throws SceneDoesNotExists {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Make unique worker name
        final String task = process.getName() + ":" + name + "@";
        // Translate the arguments
        final HashMap substitutions = getSubstitutions(args);
        // Print some information
        mLogger.message("Playing Scene '" + name + "' in process '" + process + "' on reactive player '" + this + "' with substitutions '" + substitutions.toString() + "'");

        // Get the scene object
        final SceneScript script = mProject.getSceneScript();
        String slang = null;
        // find the language used by SceneGroup
        for (String str : script.getLangSet()) {
            if (script.getSceneGroup(str, name) != null) {
                slang = str;
                break;
            }
        }
        if(slang == null){
            throw new SceneDoesNotExists(name);
        }
        final SceneGroup group = script.getSceneGroup(slang, name);
        final SceneObject scene = group.select();

        // Create playback task
        final PlayerWorker worker = new PlayerWorker(task) {

            @Override
            public void run() {
                for (SceneTurn turn : scene.getTurnList()) {
                    // Get executor for this turn
                    final ActivityExecutor turnActorExecutor = mProject.getAgentDevice(turn.getSpeaker());
                    // Serially play the utterances
                    for (SceneUttr uttr : turn.getUttrList()) {

                        //mLogger.message("Utterance " + uttr.getText().trim());
                        final LinkedList<String> textBuilder = new LinkedList();
                        final LinkedList<ActivityWorker> observedWorkerList = new LinkedList();

                        // PG 14.7.2020 add "start utterance marker
                        textBuilder.add("${'utterance':'start'}$");

                        for (final UttrElement element : uttr.getWordList()) {
                            //mLogger.message("element " + element);

                            if (element instanceof ActionObject) {
                                final ActionObject action = (ActionObject) element;
                                // Get the actor name of this action
                                final String actor = action.getActor();
                                // Get the executor for this action
                                final ActivityExecutor actionActorExecutor
                                        = (actor != null ? mProject.getAgentDevice(actor) : turnActorExecutor);
                                // Create a new marker for the action
                                // 19.4.2016 PG: CAREFULL! The marker has to be generated by the turn executor! not by the activity executor of the action!
                                //Old: final String marker = actionActorExecutor.marker(newId());
                                final String marker = turnActorExecutor.marker(newId());
                                // Append the marker to the activity
                                textBuilder.add(marker);
                                // Register the activity with marker
                                observedWorkerList.add(mScheduler.register(marker, // Execute at this marker
                                        new ActionActivity(
                                                (action.getActor() == null) ? turn.getSpeaker() : action.getActor(), // added PG 5.4.2016
                                                //action.getMode(),
                                                action.getName(),
                                                action.getText(substitutions),
                                                action.getFeatureList(),
                                                substitutions),
                                        actionActorExecutor));
                            } //else if (element instanceof SceneParam) {
                            //TODO: Get parameter from list
                            // append value of variables
                            //String var = ((SceneParam) element).getName();
                            //String val = "";
                            //try {
                            //    textBuilder.add(process.getEnvironment().read(var).getValue().toString());
                            //} catch (final InterpreterError exc) {
                            //    exc.printStackTrace();
                            //}
                            //    } 
                            else {
                                // Append the text to the activity
                                textBuilder.add(element.getText(substitutions));
                            }
                        }
                        // 
                        final String punctuation = uttr.getPunctuationMark();
                        // mLogger.message("Scheduling Speech Activity:\n" + textBuilder + "");
                        // Schedule the activity

                        // PG 14.7.2020 add "stop utterance marker
                        textBuilder.add("${'utterance':'stop'}$");

                        mScheduler.schedule(
                                0, // Schedule without delay
                                observedWorkerList,
                                new SpeechActivity(
                                        turn.getSpeaker(),
                                        textBuilder,
                                        punctuation),
                                turnActorExecutor);
                        // Check for interruption
                        if (isDone()) {
                            return;
                        }
                    }
                }
            }
        };
        // Start the playback task
        worker.start();

        // Wait for playback task
        boolean finished = false;
        while (!finished) {
            try {
                // Print some information
                //mLogger.message("Awaiting player worker '" + worker + "'");
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                //mLogger.message("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                //mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }

        // Print some information
        //mLogger.message("Continuing '" + process + "'");
    }

    // Translate the arguments
    private HashMap getSubstitutions(final List<AbstractValue> args) {
        final HashMap substitutions = new HashMap();
        if (args != null && !args.isEmpty()) {
            for (final Object object : args) {
                if (object instanceof AbstractValue) {
                    final AbstractValue value = (AbstractValue) object;
                    if (value instanceof StructValue) {
                        final StructValue struct = (StructValue) value;
                        for (Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
                            substitutions.put(entry.getKey(), entry.getValue().getValue().toString());
                            mLogger.message("SUBSITUTION " + entry.getKey() + "->" + entry.getValue().getValue().toString());
                        }
                    }
                }
            }
        }
        return substitutions;
    }
}
