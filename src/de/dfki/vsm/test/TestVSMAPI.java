//package de.dfki.vsm.test;
//
//import de.dfki.vsm.model.scenescript.UtteranceElement;
//import de.dfki.vsm.model.scenescript.ActionObject;
//import de.dfki.vsm.model.scenescript.SceneGroup;
//import de.dfki.vsm.model.scenescript.SceneObject;
//import de.dfki.vsm.model.scenescript.SceneScript;
//import de.dfki.vsm.model.scenescript.SceneTurn;
//import de.dfki.vsm.model.scenescript.SceneUttr;
//import de.dfki.vsm.runtime.RunTimeInstance;
//import de.dfki.vsm.runtime.player.RunTimePlayer;
//import de.dfki.vsm.runtime.project.RunTimeProject;
//import static de.dfki.vsm.test.TestVSMAPI.ActivityScheduler.SchedulingPolicy.BLOCKING;
//import static de.dfki.vsm.test.TestVSMAPI.ActivityScheduler.SchedulingPolicy.CONCURRENT;
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.Iterator;
//import java.util.LinkedList;
//import java.util.Scanner;
//import java.util.concurrent.Executors;
//import java.util.concurrent.ScheduledExecutorService;
//import java.util.concurrent.TimeUnit;
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
//
///**
// *
// * @author Gregor Mehlmann
// */
//public class TestVSMAPI {
//
//    // An abstract activity is the abstract representation
//    // of an activity that an agent, for example a virtual
//    // character or a robot might be able to perform. Here,
//    // an activity may be a verbal activity, that means a
//    // spoken utterance, but an activity may also be any 
//    // nonverbal performance in some other modality than
//    // speech, such as gaze, gestures, postures, facial 
//    // expressions, head movements and so on ...
//    public interface AbstractActivity {
//        
//        public String getText();
//    }
//
//    // The verbal activity is a whole utterance that the 
//    // agent has to perform using the TTS system of its
//    // underlying platform. It represents an utterance
//    // in a scene script where the nonverbal actions are
//    // replaced with markers that reference the activities
//    public static class VerbalActivity implements AbstractActivity {
//        
//        private final LinkedList<String> mList;
//        private final String mMark;
//        
//        public VerbalActivity(final LinkedList<String> list, final String mark) {
//            // Initialize the text
//            mList = list;
//            mMark = mark;
//        }
//        
//        @Override
//        public final String getText() {
//            final StringBuilder builder = new StringBuilder();
//            for (final String item : mList) {
//                builder.append(item);
//                if (!item.equals(mList.getLast())) {
//                    builder.append(' ');
//                }
//            }
//            return builder.toString();
//        }
//        
//        @Override
//        public final String toString() {
//            return getText();
//        }
//        
//    }
//
//    // A nonverbal activity represents an inner action
//    // object of an utterance in a scene script and
//    // is coupled to a unique marker which is used to
//    // substitute the action object in  speech action
//    public static class NonverbalActivity implements AbstractActivity {
//
//        /*private final String mMark;*/
//        private final String mText;
//        
//        public NonverbalActivity(/*final String mark,*/final String text) {
//            //mMark = mark;
//            mText = text;
//        }
//        
//        @Override
//        public final String getText() {
//            return mText;
//        }
//
//        //public final String getMark() {
//        //    return mMark;
//        //}
//        @Override
//        public final String toString() {
//            return getText();
//        }
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public interface AbstractTrigger {
//        
//    }
//
//    // An activity is triggered when a marker is reached
//    public static class MarkTrigger implements AbstractTrigger {
//        
//        private final String mMark;
//        
//        public MarkTrigger(final String mark) {
//            mMark = mark;
//        }
//        
//        @Override
//        public final String toString() {
//            return mMark;
//        }
//    }
//
//    // An activity is triggered when some time has expired
//    public static class TimeTrigger implements AbstractTrigger {
//        
//        private final long mDelay;
//        private final TimeUnit mUnit;
//        
//        public TimeTrigger(final long time, final TimeUnit unit) {
//            mDelay = time;
//            mUnit = unit;
//        }
//        
//        @Override
//        public final String toString() {
//            return Long.toString(mDelay) + mUnit.name();
//        }
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public interface AbstractFeedback {
//    }
//    
//    public static class FinishedFeedback implements AbstractFeedback {
//    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
////    public interface Scheduler extends Runnable {
////
////        // Determines what is delivered when to whom
////        public Scheduler register(
////                final AbstractActivity action,
////                final AbstractTrigger trigger,
////                final AbstractExecutor executor);
////
////        // Determines that a feedback has been given
////        public Scheduler feedback(
////                final AbstractActivity action,
////                final AbstractFeedback feedback);
////
////        // Determines that a trigger has been seen
////        public Scheduler trigger(final MarkTrigger trigger);
////    }
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    // The abstract command is the abstract representation
//    // of the action translated to the concrete syntax of
//    // specific platform. Abstract actions are translated
//    // to the platform-specific commands using factories.
//    public static abstract class AbstractCommand {
//
//        // The underlying abstract action
//        protected final AbstractActivity mAction;
//        
//        public AbstractCommand(final AbstractActivity action) {
//            mAction = action;
//        }
//    }
//    
////    public static abstract class CharamelCommand extends AbstractCommand {
////        
////        public CharamelCommand(final AbstractActivity action) {
////            super(action);
////        }
////        
////        @Override
////        public String toString() {
////            return "CharamelCommand(" + mAction.getText() + ")";
////        }
////    }
////    
////    public static abstract class StickmanCommand extends AbstractCommand {
////        
////        public StickmanCommand(final AbstractActivity action) {
////            super(action);
////        }
////        
////        @Override
////        public String toString() {
////            return "StickmanCommand(" + mAction.getText() + ")";
////        }
////    }
////    
////    public static class StickmanSpeech extends StickmanCommand {
////        
////        private final String mText;
////        
////        public StickmanSpeech(final AbstractActivity action) {
////            super(action);
////            // TODO: Remove markers from text of the action
////            mText = action.getText();
////        }
////        
////        private String getText() {
////            return mText;
////        }
////        
////        @Override
////        public String toString() {
////            return getText();
////        }
////    }
////    
////    public static class StickmanDummy extends StickmanCommand {
////        
////        public StickmanDummy(final AbstractActivity action) {
////            super(action);
////        }
////        
////        private String getText() {
////            return "StickmanDummy(" + mAction.getText() + ")";
////        }
////        
////        @Override
////        public String toString() {
////            return getText();
////        }
////    }
////    
////    public static class CharamelSpeech extends CharamelCommand {
////        
////        private final String mXML;
////        
////        public CharamelSpeech(final AbstractActivity action) {
////            super(action);
////            // TODO: Translate to a charamel speech command
////            mXML = action.getText();
////        }
////        
////        private String getXML() {
////            return mXML;
////        }
////        
////        @Override
////        public String toString() {
////            return getXML();
////        }
////    }
////    
////    public static class CharamelDummy extends CharamelCommand {
////        
////        public CharamelDummy(final AbstractActivity action) {
////            super(action);
////        }
////        
////        private String getText() {
////            return "CharamelDummy(" + mAction.getText() + ")";
////        }
////        
////        @Override
////        public String toString() {
////            return getText();
////        }
////    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    public interface AbstractContext {
//        
//    }
//    
//    public interface AbstractFactory {
//        
//        public AbstractCommand compile(
//                final AbstractActivity action,
//                final AbstractContext context);
//        
//    }
//    
////    public static class CharamelFactory implements AbstractFactory {
////        
////        @Override
////        public CharamelCommand compile(
////                final AbstractActivity action,
////                final AbstractContext context) {
////            if (action instanceof VerbalActivity) {
////                // Return stickman speech command
////                return new CharamelSpeech(action);
////            } else if (action instanceof NonverbalActivity) {
////                // Translate the action to command
////                // based on the context (Patrick)               
////                return new CharamelDummy(action);
////            } else {
////                return new CharamelDummy(action);
////            }
////        }
////    }
//    
////    public static class StickmanFactory implements AbstractFactory {
////        
////        @Override
////        public StickmanCommand compile(
////                final AbstractActivity action,
////                final AbstractContext context) {
////            if (action instanceof VerbalActivity) {
////                // Return stickman speech command
////                return new StickmanSpeech(action);
////            } else if (action instanceof NonverbalActivity) {
////                // Translate the action to command
////                // based on the context (Patrick)               
////                return new StickmanDummy(action);
////            } else {
////                return new StickmanDummy(action);
////            }
////        }
////    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    // An executor is responsible for the execution of an action
//    // given by a specific schedule. The executor has a factory
//    // that knows how the action is translated to some platform 
//    // specific command. The executor is then responsible for the
//    // execution of the action, which can be a direct call to the
//    // API of some animated character, or for example a packet to
//    // some server of a roboter proxy module via the network. The
//    // executor is also resonsible to forward triggers and feedback
//    // to the abstract schedule in order to inform the schedule
//    // about markers that have been detected or received as well as
//    // feddback about the execution status of actions
//    public interface AbstractExecutor {
//        
//        public String getMarker(final Long markid);
//        
//        public void execute(
//                final AbstractActivity action,
//                final ActivityScheduler scheduler);
//    }
//    
////    public static class CharamelExecutor implements AbstractExecutor, AbstractContext {
////        
////        private final CharamelFactory mFactory = new CharamelFactory();
////        
////        @Override
////        public String getMarker(final Long markid) {
////            // Acapela style bookmarks
////            return "\\mrk=" + markid + "\\";
////        }
////        
////        @Override
////        public void execute(
////                final AbstractActivity action,
////                final ActivityScheduler scheduler) {
////            // Tranlate the action
////            final CharamelCommand command = mFactory.compile(action, this);
////            // Execute the command
////            System.err.println("Charamel executing command '" + command.toString() + "'");
////            // Send command via socket ...
////        }
////    }
//    
////    public static class StickmanExecutor implements AbstractExecutor, AbstractContext {
////        
////        @Override
////        public String getMarker(final Long markid) {
////            // Microsoft style bookmarks
////            return "<mark name=\"" + markid + "\"/>";
////        }
////        
////        private final StickmanFactory mFactory = new StickmanFactory();
////        
////        @Override
////        public void execute(
////                final AbstractActivity action,
////                final ActivityScheduler scheduler) {
////            // Tranlate the action
////            final StickmanCommand command = mFactory.compile(action, this);
////            //
////            if (command instanceof StickmanSpeech) {
////                //
////                final StickmanSpeech speech = (StickmanSpeech) command;
////                // Execute speech command
////                final Scanner scanner = new Scanner(speech.getText());
////                while (scanner.hasNext("<mark name=\"(.*?)\"")) {
////                    // Get the next token
////                    final String token = scanner.next();
////                    final Pattern pattern = Pattern.compile("<mark name=\"(.*?)\"");
////                    final Matcher matcher = pattern.matcher(token);
////                    
////                    if (matcher.matches()) {
////                    // Check for bookmark
////                        //if (token.matches("<mark name=\"(.*?)\"")) {
////                        // Give some trigger now
////                        scheduler.trigger(new MarkTrigger(matcher.group(1)));
////                    } else {
////                        try {
////                            System.err.println(token);
////                            // Sleep for some time
////                            Thread.sleep(250);
////                        } catch (final Exception exc) {
////                            // Do nothing here
////                        }
////                    }
////                }
////                // Send finish feedback
////                scheduler.feedback(action, new FinishedFeedback());
////                
////            } else {
////                // ...
////            }
////            
////        }
////    }
//
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    ////////////////////////////////////////////////////////////////////////////
//    // The activity schedule is the schedule that is produced by 
//    // the scene player task from a single scene script utterance.
//    // It consists of a list of activities, which are just records
//    // holding an abstract action, the abstract trigger that can
//    // trigger that action and an abstract executor that is used
//    // to execute the action when the trigger has been detected.
//    //
//    // The activity schedule is, as the abstract schedule, a runnable
//    // and can therefore be played by a thread. The scene player task
//    // waits until this scheduler thread has finished the execution of
//    // all activities or until the scheduler has been aborted. 
//    public static final class ActivityScheduler /*extends Thread*/ {
//        
//        enum SchedulingPolicy {
//            
//            BLOCKING,
//            CONCURRENT
//        }
//        
//        public ActivityScheduler() {
//            // Do nothing here
//        }
//        
//        public void unload() {
//            //mThreadPool.shutdownNow();
//            mThreadPool.shutdownNow();
//            //
//            System.out.println("Shutting down activity scheduler");
//            
//        }
//
//        // An activity in the schedule is defined as 
//        // an action, executed by a certain executor 
//        // when a specific trigger has been detected.
//        private final class Item {
//            
//            private final SchedulingPolicy mPolicy;
//            private final AbstractTrigger mTrigger;
//            private final AbstractActivity mActivity;
//            private final AbstractExecutor mExecutor;
//            
//            private Item(
//                    final SchedulingPolicy policy,
//                    final AbstractTrigger trigger,
//                    final AbstractActivity activity,
//                    final AbstractExecutor executor) {
//                mPolicy = policy;
//                mTrigger = trigger;
//                mActivity = activity;
//                mExecutor = executor;
//            }
//        };
//
//        // A thread pool to schedule the items
//        private final ScheduledExecutorService mThreadPool
//                = Executors.newScheduledThreadPool(50);
//        // The list of activities of this plan
//        private final ArrayList<Item> mActivityList
//                = new ArrayList();
//
//        // The manager wants to register an activity
//        public final synchronized ActivityScheduler register(
//                final SchedulingPolicy policy,
//                final AbstractTrigger trigger,
//                final AbstractActivity activity,
//                final AbstractExecutor executor) {
//            // Print some debug information
//            System.err.println("Scheduling '" + activity
//                    + "' with policy '" + policy
//                    + "' and trigger '" + trigger
//                    + "' on executor '" + executor + "'");
//            // Check the activity's trigger
//            if (trigger instanceof TimeTrigger) {
//                final ActivityScheduler schedule = this;
//                mThreadPool.schedule(
//                        new Runnable() {
//                            
//                            @Override
//                            public void run() {
//                                executor.execute(activity, schedule);
//                            }
//                        },
//                        ((TimeTrigger) trigger).mDelay,
//                        ((TimeTrigger) trigger).mUnit);
//            } else if (trigger instanceof MarkTrigger) {
//                // Create and add the new activity
//                mActivityList.add(
//                        new Item(policy, trigger, activity, executor));
//            }
//            //
//            return this;
//        }
//
//        // The executor gives a new detected trigger
//        public final synchronized ActivityScheduler trigger(final MarkTrigger trigger) {
//            System.err.println("Detecting trigger '" + trigger.toString() + "'");
//            final Iterator it = mActivityList.iterator();
//            while (it.hasNext()) {
//                final Item activity = (Item) it.next();
//                // If this activity has the detected trigger
//                // then schedule it immediately without delay
//                if (activity.mTrigger.toString().equals(trigger.toString())) {
//                    
//                    final ActivityScheduler schedule = this;
//                    mThreadPool.schedule(
//                            new Runnable() {
//                                
//                                @Override
//                                public void run() {
//                                    activity.mExecutor.execute(activity.mActivity, schedule);
//                                }
//                            },
//                            0, TimeUnit.MILLISECONDS);
//                }
//            }
//            //
//            return this;
//        }
//
//        // The executor gives a feedback to a command
//        public synchronized final ActivityScheduler feedback(
//                final AbstractActivity action,
//                final AbstractFeedback feedback) {
//            final Iterator it = mActivityList.iterator();
//            while (it.hasNext()) {
//                final Item activity = (Item) it.next();
//                // If this activity has the given command and
//                // the feedback is finished then remove it now
//                if (activity.mActivity.equals(action)) {
//                    if (feedback instanceof FinishedFeedback) {
//                        it.remove();
//                    }
//                }
//            }
//            // Check if list empty and notify the 
//            // scheduler thread that executes the
//            // schedule that the schedule is done
//            // if the activity list is empty now
//            if (mActivityList.isEmpty()) {
//                notify();
//            }
//            //
//            return this;
//        }
//
//        // Iterate over the
////        private synchronized void run() {
////            final Iterator it = mActivityList.iterator();
////            while (it.hasNext()) {
////                final Item activity = (Item) it.next();
////                if (activity.mTrigger instanceof TimeTrigger) {
////                    // Get the timestamp of the trigger
////                    final long delay = ((TimeTrigger) activity.mTrigger).mTime;
////                    // Schedule the activity accordingly
////                    schedule(activity, delay);
////                    // And finally remove the activity
////                    it.remove();
////                }
////            }
////            System.out.println("Starting reaction to action triggers/feedbacks");
////            //
////            while (!mActivityList.isEmpty()) {
////                try {
////                    // Wait for notification
////                    wait();
////                } catch (final Exception exc) {
////                    // Abort the scheduler
////                    // TODO:
////                }
////            }
////
////            System.out.println("Stopping execution of the activity scheduler");
////        }
//    }
//
//    // The scene player
//    public static final class GenericScenePlayer implements RunTimePlayer {
//
//        // The static marker id
//        private static Long sMarkId = 0x0L;
//
//        // Get unique marker id
//        private static Long newMarkId() {
//            return ++sMarkId;
//        }
//
//        // The runtime environment
//        private final RunTimeInstance mRunTime
//                = RunTimeInstance.getInstance();
//        // The defaut system logger
//        private final LOGDefaultLogger mLogger
//                = LOGDefaultLogger.getInstance();
//
//        // The runtime project data
//        private RunTimeProject mProject;
//        
//        private final HashMap<String, AbstractExecutor> mExecutorMap = new HashMap();
//        
//        private final ActivityScheduler mScheduler = new ActivityScheduler();
//        
//        @Override
//        public final boolean launch(final RunTimeProject project) {
//            // Initialize the project
//            mProject = project;
//            // Initialize the executors
//            //mExecutorMap.put("reeti", new StickmanExecutor());
//            //mExecutorMap.put("naoli", new CharamelExecutor());
//            // Start the scheduler now
//            //mScheduler.start();
//
//            // Return true at success
//            return true;
//        }
//        
//        @Override
//        public final boolean unload() {
//            //
//            mScheduler.unload();
//            System.out.println("Shutting down generic scene player");
//            // Return true at success
//            return true;
//        }
//        
//        public final void act(final String actor, final String action) {
//            // Get the correct executor
//            final AbstractExecutor executor = mExecutorMap.get(actor);
//            
//        }
//        
//        @Override
//        public void play(
//                final String name,
//                final LinkedList args) {
//            play(name, "en", args);
//        }
//
//        //@Override
//        public final void play(
//                final String name,
//                final String lang,
//                final LinkedList args) {
//            // Print some debug information
//            mLogger.message(name);
//            // Translate the argument list
//            final HashMap map = new HashMap();
//            // Get the scene language group 
//            final SceneScript script = mProject.getSceneScript();
//            final SceneGroup group = script.getSceneGroup(lang, name);
//            // Select a new scene
//            final SceneObject scene = group.select();
//            // Process the scene
//            for (SceneTurn turn : scene.getTurnList()) {
//                // Get the executor for this turn
//                final AbstractExecutor turnExecutor = mExecutorMap.get(turn.getSpeaker());
//                //
//                for (SceneUttr uttr : turn.getUttrList()) {
//                    //
//                    final LinkedList<String> builder = new LinkedList();
//                    final ActivityScheduler schedule = new ActivityScheduler();
//                    //
//                    for (final UtteranceElement element : uttr.getWordList()) {
//                        if (element instanceof ActionObject) {
//                            final ActionObject action = (ActionObject) element;
//                            // Get the actor name of this action
//                            final String actor = action.getActorName();
//                            // Get the executor for this action
//                            final AbstractExecutor actionExecutor
//                                    = (actor != null ? mExecutorMap.get(actor) : turnExecutor);
//                            // Create a new marker for the action
//                            final String marker = actionExecutor.getMarker(newMarkId());
//                            // Append the marker to the activity
//                            builder.add(marker);
//                            // Register the activity with marker
//                            schedule.register(
//                                    CONCURRENT,
//                                    new MarkTrigger(marker),
//                                    new NonverbalActivity(action.getText(map)),
//                                    actionExecutor);
//                        } else {
//                            // Append the text to the activity
//                            builder.add(element.getText(map));
//                        }
//                    }
//                    // 
//                    final String mark = uttr.getPunctuationMark();
//                    //
//                    schedule.register(
//                            BLOCKING,
//                            new TimeTrigger(0, TimeUnit.MILLISECONDS),
//                            new VerbalActivity(builder, mark),
//                            turnExecutor);
//                }
//            }
//        }
//    }
//    
//    public static void main(final String[] args) {
//        //System.err.println("Initializing test suite");
//        // Make a new runtime project for testing
//        final RunTimeProject project = new RunTimeProject();
//        // Try to parse the project from file name
//        if (project.parse(args[0])) {
//            // Make a generic sceneplayer for testing
//            final GenericScenePlayer player = new GenericScenePlayer();
//            // Lauch the scene player with the project
//            if (player.launch(project)) {
//                // And play some of the project's scenes
//                player.play("TestScene", new LinkedList());
//                // Unload the player after the playbacks
//                if (player.unload()) {
//                    // 
//                }
//            }
//        }
//        //System.err.println("Terminating test suite");
//
////        // The main function represents the 
////        // scene player task within the play
////        // method of the scene player instance
////        final StickmanExecutor stickmanExecutor = new StickmanExecutor();
////        final CharamelExecutor charamelExecutor = new CharamelExecutor();
////        // Create an activity scheduler
////        final ActivitySchedule schedule = new ActivitySchedule();
////        // Register some new actions at the
////        // activity schedule using builder 
////        // patterns of the member functions
////        schedule.
////                register(
////                        new VerbalActivity("Hello charamel #1 how #2 #3 are you today?"),
////                        new TimeTrigger(1000), stickmanExecutor).
////                //                register(
////                //                        new SpeechAction("#4 stickman I guess I am kind of #5 fine!"),
////                //                        new TimeTrigger(3000), charamelExecutor).
////                register(
////                        new NonverbalActivity("[Reeti: head nod repetitions=2]"),
////                        new MarkTrigger("#1"), charamelExecutor).
////                register(
////                        new NonverbalActivity("[Naoli: gaze target=Gregor]"),
////                        new MarkTrigger("#2"), charamelExecutor);
////        // Create a scheduler
////        final Thread scheduler = new Thread(schedule);
////        // Start the sceduler
////        scheduler.start();
////        // Wait for scheduler
////        try {
////            scheduler.join();
////        } catch (final Exception exc) {
////            // Do nothing here
////        }
//        // Proceed with the next utterance in the scene script here ...
//    }
//}
