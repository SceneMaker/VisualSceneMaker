package de.dfki.vsm.test;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;
import java.util.Timer;
import java.util.TimerTask;

/**
 *
 * @author Gregor Mehlmann
 */
public class TestVSMAPI {

    // The abstract action is the abstract representation
    // of an action that an agent, for example a virtual
    // character or robot might be able to execute. 
    public interface AbstractAction {

        public String getText();
    }

    // The speech action is a whole utterance that the 
    // agent has to perform using the TTS system of its
    // underlying platform. It represents an utterance
    // in a scene script where the action objects are
    // replaced with markers that reference the actions
    public static class SpeechAction implements AbstractAction {

        private final String mText;

        public SpeechAction(final String text) {
            mText = text;
        }

        @Override
        public final String getText() {
            return mText;
        }

        @Override
        public final String toString() {
            return getText();
        }

    }

    // An inner action represents an inner action
    // object of an utterance in a scene script and
    // is coupled to a unique marker which is used to
    // substitute the action object in  speech action
    public static class InnerAction implements AbstractAction {

        /*private final String mMark;*/
        private final String mText;

        public InnerAction(/*final String mark,*/final String text) {
            //mMark = mark;
            mText = text;
        }

        @Override
        public final String getText() {
            return mText;
        }

        //public final String getMark() {
        //    return mMark;
        //}
        @Override
        public final String toString() {
            return getText();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public interface AbstractTrigger {

    }

    // An activity is triggered when a marker is reached
    public static class MarkTrigger implements AbstractTrigger {

        private final String mMark;

        public MarkTrigger(final String mark) {
            mMark = mark;
        }

        @Override
        public final String toString() {
            return mMark;
        }
    }

    // An activity is triggered when some time has expired
    public static class TimeTrigger implements AbstractTrigger {

        private final long mTime;

        public TimeTrigger(final long time) {
            mTime = time;
        }

        @Override
        public final String toString() {
            return Long.toString(mTime);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public interface AbstractFeedback {
    }

    public static class FinishedFeedback implements AbstractFeedback {
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public interface AbstractSchedule extends Runnable {

        // Determines what is delivered when to whom
        public AbstractSchedule register(
                final AbstractAction action,
                final AbstractTrigger trigger,
                final AbstractExecutor executor);

        // Determines that a feedback has been given
        public AbstractSchedule feedback(
                final AbstractAction action,
                final AbstractFeedback feedback);

        // Determines that a trigger has been seen
        public AbstractSchedule trigger(final MarkTrigger trigger);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // The abstract command is the abstract representation
    // of the action translated to the concrete syntax of
    // specific platform. Abstract actions are translated
    // to the platform-specific commands using factories.
    public static abstract class AbstractCommand {

        // The underlying abstract action
        protected final AbstractAction mAction;

        public AbstractCommand(final AbstractAction action) {
            mAction = action;
        }
    }

    public static abstract class CharamelCommand extends AbstractCommand {

        public CharamelCommand(final AbstractAction action) {
            super(action);
        }

        @Override
        public String toString() {
            return "CharamelCommand(" + mAction.getText() + ")";
        }
    }

    public static abstract class StickmanCommand extends AbstractCommand {

        public StickmanCommand(final AbstractAction action) {
            super(action);
        }

        @Override
        public String toString() {
            return "StickmanCommand(" + mAction.getText() + ")";
        }
    }

    public static class StickmanSpeech extends StickmanCommand {

        private final String mText;

        public StickmanSpeech(final AbstractAction action) {
            super(action);
            // TODO: Remove markers from text of the action
            mText = action.getText();
        }

        private String getText() {
            return mText;
        }

        @Override
        public String toString() {
            return getText();
        }
    }

    public static class StickmanDummy extends StickmanCommand {

        public StickmanDummy(final AbstractAction action) {
            super(action);
        }

        private String getText() {
            return "StickmanDummy(" + mAction.getText() + ")";
        }

        @Override
        public String toString() {
            return getText();
        }
    }

    public static class CharamelSpeech extends CharamelCommand {

        private final String mXML;

        public CharamelSpeech(final AbstractAction action) {
            super(action);
            // TODO: Translate to a charamel speech command
            mXML = action.getText();
        }

        private String getXML() {
            return mXML;
        }

        @Override
        public String toString() {
            return getXML();
        }
    }

    public static class CharamelDummy extends CharamelCommand {

        public CharamelDummy(final AbstractAction action) {
            super(action);
        }

        private String getText() {
            return "CharamelDummy(" + mAction.getText() + ")";
        }

        @Override
        public String toString() {
            return getText();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public interface AbstractContext {

    }

    public interface AbstractFactory {

        public AbstractCommand compile(
                final AbstractAction action,
                final AbstractContext context);

    }

    public static class CharamelFactory implements AbstractFactory {

        @Override
        public CharamelCommand compile(
                final AbstractAction action,
                final AbstractContext context) {
            if (action instanceof SpeechAction) {
                // Return stickman speech command
                return new CharamelSpeech(action);
            } else if (action instanceof InnerAction) {
                // Translate the action to command
                // based on the context (Patrick)               
                return new CharamelDummy(action);
            } else {
                return new CharamelDummy(action);
            }
        }
    }

    public static class StickmanFactory implements AbstractFactory {

        @Override
        public StickmanCommand compile(
                final AbstractAction action,
                final AbstractContext context) {
            if (action instanceof SpeechAction) {
                // Return stickman speech command
                return new StickmanSpeech(action);
            } else if (action instanceof InnerAction) {
                // Translate the action to command
                // based on the context (Patrick)               
                return new StickmanDummy(action);
            } else {
                return new StickmanDummy(action);
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // An executor is responsible for the execution of an action
    // given by a specific schedule. The executor has a factory
    // that knows how the action is translated to some platform 
    // specific command. The executor is then responsible for the
    // execution of the action, which can be a direct call to the
    // API of some animated character, or for example a packet to
    // some server of a roboter proxy module via the network. The
    // executor is also resonsible to forward triggers and feedback
    // to the abstract schedule in order to inform the schedule
    // about markers that have been detected or received as well as
    // feddback about the execution status of actions
    public interface AbstractExecutor {

        public void execute(
                final AbstractAction action,
                final AbstractSchedule scheduler);
    }

    public static class CharamelExecutor implements AbstractExecutor, AbstractContext {

        private final CharamelFactory mFactory = new CharamelFactory();

        @Override
        public void execute(
                final AbstractAction action,
                final AbstractSchedule scheduler) {
            // Tranlate the action
            final CharamelCommand command = mFactory.compile(action, this);
            // Execute the command
            System.err.println("Charamel executing command '" + command.toString() + "'");
            // Send command via socket ...
        }
    }

    public static class StickmanExecutor implements AbstractExecutor, AbstractContext {

        private final StickmanFactory mFactory = new StickmanFactory();

        @Override
        public void execute(
                final AbstractAction action,
                final AbstractSchedule scheduler) {
            // Tranlate the action
            final StickmanCommand command = mFactory.compile(action, this);
            //
            if (command instanceof StickmanSpeech) {
                //
                final StickmanSpeech speech = (StickmanSpeech) command;
                // Execute speech command
                final Scanner scanner = new Scanner(speech.getText());
                while (scanner.hasNext()) {
                    // Get the next token
                    final String token = scanner.next();
                    // Check for bookmark
                    if (token.matches("#(.*?)")) {
                        // Give some trigger now
                        scheduler.trigger(new MarkTrigger(token));
                    } else {
                        try {
                            System.err.println(token);
                            // Sleep for some time
                            Thread.sleep(250);
                        } catch (final Exception exc) {
                            // Do nothing here
                        }
                    }
                }
                // Send finish feedback
                scheduler.feedback(action, new FinishedFeedback());

            } else {
                // ...
            }

        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    // The activity schedule is the schedule that is produced by 
    // the scene player task from a single scene script utterance.
    // It consists of a list of activities, which are just records
    // holding an abstract action, the abstract trigger that can
    // trigger that action and an abstract executor that is used
    // to execute the action when the trigger has been detected.
    //
    // The activity schedule is, as the abstract schedule, a runnable
    // and can therefore be played by a thread. The scene player task
    // waits until this scheduler thread has finished the execution of
    // all activities or until the scheduler has been aborted. 
    public static final class ActivitySchedule implements AbstractSchedule {

        public ActivitySchedule() {
            // Do nothing here
        }

        // An activity in the schedule is defined as 
        // an action, executed by a certain executor 
        // when a specific trigger has been detected.
        private final class Activity {

            private final AbstractAction mAction;
            private final AbstractTrigger mTrigger;
            private final AbstractExecutor mExecutor;

            private Activity(
                    final AbstractAction action,
                    final AbstractTrigger trigger,
                    final AbstractExecutor executor) {
                mAction = action;
                mTrigger = trigger;
                mExecutor = executor;
            }
        };

        // A timer to schedule the activities
        private final Timer mActivityTimer = new Timer();
        // The list of activities of this plan
        private final ArrayList<Activity> mActivityList
                = new ArrayList<Activity>();

        // The manager wants to register an activity
        @Override
        public final synchronized ActivitySchedule register(
                final AbstractAction action,
                final AbstractTrigger trigger,
                final AbstractExecutor executor) {
            // Create and add the new activity
            mActivityList.add(
                    new Activity(action, trigger, executor));
            //
            return this;
        }

        // The executor gives a new detected trigger
        @Override
        public final synchronized ActivitySchedule trigger(final MarkTrigger trigger) {
            System.err.println("Detecting trigger '" + trigger.toString() + "'");
            final Iterator it = mActivityList.iterator();
            while (it.hasNext()) {
                final Activity activity = (Activity) it.next();
                // If this activity has the detected trigger
                // then schedule it immediately without delay
                if (activity.mTrigger.toString().equals(trigger.toString())) {
                    activity.mExecutor.execute(activity.mAction, this);
                }
            }
            //
            return this;
        }

        // The executor gives a feedback to a command
        @Override
        public synchronized final ActivitySchedule feedback(
                final AbstractAction action,
                final AbstractFeedback feedback) {
            final Iterator it = mActivityList.iterator();
            while (it.hasNext()) {
                final Activity activity = (Activity) it.next();
                // If this activity has the given command and
                // the feedback is finished then remove it now
                if (activity.mAction.equals(action)) {
                    if (feedback instanceof FinishedFeedback) {
                        it.remove();
                    }
                }
            }
            // Check if list empty and notify the 
            // scheduler thread that executes the
            // schedule that the schedule is done
            // if the activity list is empty now
            if (mActivityList.isEmpty()) {
                notify();
            }
            //
            return this;
        }

        // Schedule the future execution an activity 
        private void schedule(final Activity activity, final long delay) {
            final AbstractSchedule scheduler = this;
            mActivityTimer.schedule(new TimerTask() {

                @Override
                public void run() {
                    activity.mExecutor.execute(activity.mAction, scheduler);
                }
            }, delay);
        }

        // Run this activity scheduler until the plan has 
        // been completely worked off. First the scheduler 
        // schedules the future executions of the activities
        // that have a timestamp as trigger. Afterwards is
        // goes asleep waiting until the list with activities 
        // is empty. The scheduler is waked up to check this
        // condition whenever an activity has been removed
        @Override
        public synchronized void run() {
            System.out.println("Starting execution of the activity scheduler");

            System.out.println("Starting scheduling of timed action executions");
            final Iterator it = mActivityList.iterator();
            while (it.hasNext()) {
                final Activity activity = (Activity) it.next();
                if (activity.mTrigger instanceof TimeTrigger) {
                    // Get the timestamp of the trigger
                    final long delay = ((TimeTrigger) activity.mTrigger).mTime;
                    // Schedule the activity accordingly
                    schedule(activity, delay);
                    // And finally remove the activity
                    it.remove();
                }
            }
            System.out.println("Starting reaction to action triggers/feedbacks");
            //
            while (!mActivityList.isEmpty()) {
                try {
                    // Wait for notification
                    wait();
                } catch (final Exception exc) {
                    // Abort the scheduler
                    // TODO:
                }
            }

            System.out.println("Stopping execution of the activity scheduler");
        }
    }

    public static void main(final String[] args) {
        // The main function represents the 
        // scene player task within the play
        // method of the scene player instance

        // Create the different executors
        final StickmanExecutor stickmanExecutor = new StickmanExecutor();
        final CharamelExecutor charamelExecutor = new CharamelExecutor();
        // Create an activity scheduler
        final ActivitySchedule schedule = new ActivitySchedule();
        // Register some new actions at the
        // activity schedule using builder 
        // patterns of the member functions
        schedule.
                register(
                        new SpeechAction("Hello charamel #1 how #2 #3 are you today?"),
                        new TimeTrigger(1000), stickmanExecutor).
//                register(
//                        new SpeechAction("#4 stickman I guess I am kind of #5 fine!"),
//                        new TimeTrigger(3000), charamelExecutor).
                register(
                        new InnerAction("[Reeti: head nod repetitions=2]"),
                        new MarkTrigger("#1"), charamelExecutor).
                register(
                        new InnerAction("[Naoli: gaze target=Gregor]"),
                        new MarkTrigger("#2"), charamelExecutor);

        // Create a scheduler
        final Thread scheduler = new Thread(schedule);
        // Start the sceduler
        scheduler.start();
        // Wait for scheduler
        try {
            scheduler.join();
        } catch (final Exception exc) {
            // Do nothing here
        }
        // Proceed with the next utterance in the scene script here ...
    }
}
