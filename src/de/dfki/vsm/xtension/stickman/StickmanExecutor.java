package de.dfki.vsm.xtension.stickman;

import de.dfki.action.sequence.TimeMark;
import de.dfki.action.sequence.Word;
import de.dfki.action.sequence.WordTimeMarkSequence;

import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.stickman3D.Stickman3D;
import de.dfki.util.ios.IOSIndentWriter;
import de.dfki.util.xml.XMLUtilities;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.stickman.StickmanRepository;

import java.io.ByteArrayOutputStream;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;

import de.dfki.vsm.xtension.stickmantts.util.property.StickmanTTSProjectProperty;
import java.io.File;
import javafx.scene.paint.Color;

/**
 *
 * @author Patrick Gebhard
 */
public class StickmanExecutor extends ActivityExecutor implements ExportableProperties {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The tworld listener
    private StickmanListener mListener;
    // The client thread list
    private final HashMap<String, StickmanHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    private Thread stickmanLaunchThread;
    private StageRoom stickmanStageC;
    private StickmanRepository stickmanFactory;
    public static HashMap<String, Stickman3D> stickmanContainer = new HashMap<>();
    private ExportableProperties exportableProperties = new StickmanTTSProjectProperty();

    // Construct the executor
    public StickmanExecutor(final PluginConfig config, final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        stickmanFactory = new StickmanRepository(config);

    }

    // Accept some socket
    public void accept(final Socket socket) {
        // Make new client thread
        final StickmanHandler client = new StickmanHandler(socket, this);
        // Add the client to list
        // TODO: Get some reasonable name for references here!
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        //
        mLogger.warning("Accepting " + client.getName() + "");
    }

    @Override
    public final String marker(final long id) {
        // Stickman style bookmarks
        return "$" + id;
    }

    @Override
    public void execute(AbstractActivity activity/*, ActivityScheduler scheduler*/) {
        // get action information
        final String actor = activity.getActor();
        final String name = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatures();

        Animation stickmanAnimation;

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;

            String activityText = sa.getTextOnly("$").trim();
            if (activityText.isEmpty()) {
                LinkedList<String> timemarks = sa.getTimeMarks("$");
                for (String tm : timemarks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                // create a new word time mark sequence based on the current utterance blocks
                WordTimeMarkSequence wts = new WordTimeMarkSequence(sa.getTextOnly("$"));

                LinkedList blocks = sa.getBlocks();
                for (final Object item : blocks) {
                    if (!item.toString().contains("$")) {
                        wts.add(new Word(item.toString()));
                    } else {
                        wts.add(new TimeMark(item.toString()));
                    }
                }

                // schedule Mouth_open and Mouth closed activities
                mScheduler.schedule(20, null, new ActionActivity(actor, /*"face",*/ "Mouth_O", null, null, null), mProject.getAgentDevice(actor));
                mScheduler.schedule(200, null, new ActionActivity(actor, /*"face",*/ "Mouth_Default", null, null, null), mProject.getAgentDevice(actor));
                stickmanAnimation = stickmanFactory.loadEventAnimation(stickmanStageC.getStickman(actor), "Speaking", 3000, false);
                stickmanAnimation.setParameter(wts);
                executeAnimationAndWait(activity, stickmanAnimation);
            }
        } else if (activity instanceof ActionActivity) {
            stickmanAnimation = stickmanFactory.loadAnimation(stickmanStageC.getStickman(actor), name, 500, false); // TODO: with regard to get a "good" timing, consult the gesticon
            if (stickmanAnimation != null) {
                if (features != null && !(features.isEmpty())) {
                    for (final ActionFeature feature : features) {
                        if (!feature.getVal().isEmpty()) {
                            if (isFloat(feature.getVal())) {
                                stickmanAnimation.setParameter(feature.getVal().replace("'", ""));
                            } else if (feature.getVal().contains(".")) {
                                String filepath = "res" + File.separator + "background";
                                String fileAbsolutePath = new File(filepath).getAbsolutePath();
                                String sfileAbsolutePath = fileAbsolutePath + File.separator + feature.getVal().replace("'", "");
                                stickmanAnimation.setParameter(sfileAbsolutePath);
                            } else {
                                stickmanAnimation.setParameter(feature.getVal().replace("'", ""));
                            }
                        }
                    }
                }
                System.out.println("de.dfki.vsm.xtension.stickman.StickmanExecutor.execute()");
                executeAnimation(stickmanAnimation);
            }
        }
    }

    private boolean isFloat(String input) {
        try {
            Float.parseFloat(input);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    private void executeAnimation(Animation stickmanAnimation) {
        // executeAnimation command to platform
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(stickmanAnimation, iosw);
        broadcast(out.toString().replace("\n", " "));
    }

    private void executeAnimationAndWait(AbstractActivity activity, Animation stickmanAnimation) {
        // executeAnimation command to platform
        synchronized (mActivityWorkerMap) {
            // executeAnimation command to platform
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            IOSIndentWriter iosw = new IOSIndentWriter(out);
            boolean r = XMLUtilities.writeToXMLWriter(stickmanAnimation, iosw);
            broadcast(out.toString().replace("\n", " "));

            // organize wait for feedback
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(stickmanAnimation.getmID(), cAW);

            // wait until we got feedback
            mLogger.message("ActivityWorker " + stickmanAnimation.getmID() + " waiting ....");
            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }
            mLogger.message("ActivityWorker " + stickmanAnimation.getmID() + "  done ....");
        }
    }

    @Override
    public void launch() {
        // read config
        final String host = mConfig.getProperty("smhost");
        final String port = mConfig.getProperty("smport");

        // Create the connection
        mListener = new StickmanListener(Integer.parseInt(port), this);
        // Start the connection
        mListener.start();

        final boolean showStickmanNames = mConfig.containsKey("showstickmanname") ? mConfig.getProperty("showstickmanname").equalsIgnoreCase("true") : true;

        // Start the StickmanStage client application
        mLogger.message("Starting StickmanStage Client Application ...");
        stickmanStageC = stickmanFactory.createStickman();
        // Get Stickman agents configuration
        for (String name : mProject.getAgentNames()) {
//            AgentConfig ac = mProject.getAgentConfig(name);
//            if (ac.getDeviceName().equalsIgnoreCase("stickman")) {
            stickmanStageC.addStickman(name);
//            }
        }

        stickmanLaunchThread = new Thread() {
            public void run() {
                try {
                    stickmanStageC.launchStage(true, mProject.getProjectPath());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };

        stickmanLaunchThread.start();

        // Wait for stickman stage to be initialized
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for StickmanStage");
            try {
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {
                mLogger.failure("Error while waiting ...");
            }
        }
    }

    @Override
    public void unload() {
        // clear the stage
        stickmanStageC.clearStage();
        //stickmanLaunchThread.interrupt();
        // Abort the client threads
        for (final StickmanHandler client : mClientMap.values()) {
            client.abort();
            // Join the client thread
            try {
                client.join();
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
                // Print some information 
                mLogger.message("Joining client thread");
            }
        }
        // Clear the map of clients 
        mClientMap.clear();
        // Abort the server thread
        try {
            mListener.abort();
            // Join the client thread
            mListener.join();
            // Print some information 
            mLogger.message("Joining server thread");
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }
    }

    // Handle some message
    public void handle(final String message, final StickmanHandler client) {
        mLogger.message("Handling " + message + "");

        synchronized (mActivityWorkerMap) {

            if (message.contains("#ANIM#end#")) {
                int start = message.lastIndexOf("#") + 1;
                String animId = message.substring(start);

                if (mActivityWorkerMap.containsKey(animId)) {
                    mActivityWorkerMap.remove(animId);
                }
                // wake me up ..
                mActivityWorkerMap.notifyAll();
            } else if (message.contains("$")) {
                // wake me up ..
                mActivityWorkerMap.notifyAll();
                // identify the related activity
                //AbstractActivity activity = mProject.getRunTimePlayer().getActivityScheduler().getMarkerActivity(message);
                // play the activity
                //mProject.getRunTimePlayer().getActivityScheduler().handle(new MarkerFeedback(activity, message));
                mProject.getRunTimePlayer().getActivityScheduler().handle(message);
            }
        }
    }

    // Broadcast some message
    private void broadcast(final String message) {
        for (final StickmanHandler client : mClientMap.values()) {
            client.send(message);
        }
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties.getExportableProperties();
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return exportableProperties.getExportableAgentProperties();
    }
}
