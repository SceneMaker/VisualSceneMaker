package de.dfki.vsm.xtension.reeti;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.PauseActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.tworld.xml.feedback.TWorldFeedback;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class ReetiExecutor extends ActivityExecutor {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mWorkerMap
            = new HashMap();

    // Construct the executor
    public ReetiExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
    }

    // Launch the executor 
    @Override
    public void launch() {
        // Get the plugin configuration
        final String host = mConfig.getProperty("host");
        final String port = mConfig.getProperty("port");
    }

    // Unload the executor 
    @Override
    public void unload() {

    }

    @Override
    public synchronized final String marker(final long id) {
        // TWorld style bookmarks
        return "\\book=" + id + "";
    }

    @Override
    public final void execute(final AbstractActivity activity) {
        // Get action information
        final String name = activity.getName();
        final String mode = activity.getMode();
        final String type = activity.getType().name();
        final String actor = activity.getActor();
        final LinkedList<ActionFeature> features = activity.getFeatureList();

        if (activity instanceof SpeechActivity) {

        } else if (activity instanceof PauseActivity) {

        } else if (activity instanceof ActionActivity) {

        } else {
        }

        mLogger.message("Building command " + name + " for Actor " + activity.getActor());
        // finalize build command

        // send command to platform 
        synchronized (mWorkerMap) {

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            ActivityWorker worker = (ActivityWorker) Thread.currentThread();
            mWorkerMap.put(worker.getName(), worker);

            // wait until we got feedback
            mLogger.warning("ActivityWorker " + worker.getName() + " waiting ....");

            while (mWorkerMap.containsValue(worker)) {
                try {
                    mWorkerMap.wait();
                } catch (final InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }

            mLogger.warning("ActivityWorker " + worker.getName() + " done ....");
        }
        // Return when terminated
    }
}
