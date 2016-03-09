package de.dfki.vsm.runtime.player.scheduler;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.executor.AbstractExecutor;
import de.dfki.vsm.runtime.player.feedback.AbstractFeedback;
import de.dfki.vsm.runtime.player.feedback.StatusFeedback;
import de.dfki.vsm.runtime.player.feedback.TriggerFeedback;
import de.dfki.vsm.runtime.player.trigger.AbstractTrigger;
import de.dfki.vsm.runtime.player.trigger.MarkTrigger;
import de.dfki.vsm.runtime.player.trigger.TimeTrigger;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.ArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityScheduler implements AbstractScheduler {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // An activity in the schedule is defined as 
    // an action, executed by a certain executor 
    // when a specific trigger has been detected.
    private final class Item {

        private final SchedulingPolicy mPolicy;
        private final AbstractTrigger mTrigger;
        private final AbstractActivity mActivity;
        private final AbstractExecutor mExecutor;

        private Item(
                final SchedulingPolicy policy,
                final AbstractTrigger trigger,
                final AbstractActivity activity,
                final AbstractExecutor executor) {
            mPolicy = policy;
            mTrigger = trigger;
            mActivity = activity;
            mExecutor = executor;
        }
    };

    // A thread pool to schedule the items
    private final ScheduledExecutorService mThreadPool
            = Executors.newScheduledThreadPool(50);
    // The list of activities of this plan
    private final ArrayList<Item> mActivityList
            = new ArrayList();

    public ActivityScheduler() {
    }

    @Override
    public final synchronized boolean launch() {
        //
        mLogger.message("Launching activity scheduler");
        //
        return true;
    }

    @Override
    public final synchronized boolean unload() {
        //
        mThreadPool.shutdown();
        //
        mLogger.message("Unloading activity scheduler");
        // 
        return true;
    }

    @Override
    public final synchronized void feedback(final AbstractFeedback feedback) {
        //
        if (feedback instanceof StatusFeedback) {
            // Get the status message
            final String status = ((StatusFeedback) feedback).getStatus();
        } else if (feedback instanceof TriggerFeedback) {
            // Get the trigger object
            final AbstractTrigger trigger = ((TriggerFeedback) feedback).getTrigger();
            // Parse the trigger now
            if (trigger instanceof MarkTrigger) {
                //
                final String mark = ((MarkTrigger) trigger).toString();
            } else if (trigger instanceof TimeTrigger) {
                //
                final Long delay = ((TimeTrigger) trigger).getDelay();
                final TimeUnit unit = ((TimeTrigger) trigger).getUnit();
            } else {
                //
            }
        } else {
            //
        }
    }

    // The manager wants to register an activity
    @Override
    public final synchronized AbstractScheduler register(
            final SchedulingPolicy policy,
            final AbstractTrigger trigger,
            final AbstractActivity activity,
            final AbstractExecutor executor) {
        // Print some debug information
        System.err.println("Registering '" + activity
                + "' with policy '" + policy
                + "' and trigger '" + trigger
                + "' on executor '" + executor + "'");
        // Check the activity's trigger
        if (trigger instanceof TimeTrigger) {
            final AbstractScheduler schedule = this;
            mThreadPool.schedule(
                    new Runnable() {

                        @Override
                        public void run() {
                            executor.execute(activity, schedule);
                        }
                    },
                    ((TimeTrigger) trigger).getDelay(),
                    ((TimeTrigger) trigger).getUnit());
        } else if (trigger instanceof MarkTrigger) {
            // Create and add the new activity
            mActivityList.add(
                    new Item(policy, trigger, activity, executor));
        }
        //
        return this;
    }

    /*
     // The executor gives a new detected trigger
     public final synchronized ActivityScheduler trigger(final MarkTrigger trigger) {
     System.err.println("Detecting trigger '" + trigger.toString() + "'");
     final Iterator it = mActivityList.iterator();
     while (it.hasNext()) {
     final Item activity = (Item) it.next();
     // If this activity has the detected trigger
     // then schedule it immediately without delay
     if (activity.mTrigger.toString().equals(trigger.toString())) {

     final ActivityScheduler schedule = this;
     mThreadPool.schedule(
     new Runnable() {

     @Override
     public void run() {
     activity.mExecutor.execute(activity.mActivity, schedule);
     }
     },
     0, TimeUnit.MILLISECONDS);
     }
     }
     //
     return this;
     }

     // The executor gives a feedback to a command
     public synchronized final ActivityScheduler feedback(
     final AbstractActivity action,
     final AbstractFeedback feedback) {
     final Iterator it = mActivityList.iterator();
     while (it.hasNext()) {
     final Item activity = (Item) it.next();
     // If this activity has the given command and
     // the feedback is finished then remove it now
     if (activity.mActivity.equals(action)) {
     if (feedback instanceof TestVSMAPI.FinishedFeedback) {
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
     */
    // Iterate over the
//        private synchronized void run() {
//            final Iterator it = mActivityList.iterator();
//            while (it.hasNext()) {
//                final Item activity = (Item) it.next();
//                if (activity.mTrigger instanceof TimeTrigger) {
//                    // Get the timestamp of the trigger
//                    final long delay = ((TimeTrigger) activity.mTrigger).mTime;
//                    // Schedule the activity accordingly
//                    schedule(activity, delay);
//                    // And finally remove the activity
//                    it.remove();
//                }
//            }
//            System.out.println("Starting reaction to action triggers/feedbacks");
//            //
//            while (!mActivityList.isEmpty()) {
//                try {
//                    // Wait for notification
//                    wait();
//                } catch (final Exception exc) {
//                    // Abort the scheduler
//                    // TODO:
//                }
//            }
//
//            System.out.println("Stopping execution of the activity scheduler");
//        }
}
