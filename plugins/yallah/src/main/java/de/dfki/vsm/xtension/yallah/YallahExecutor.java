package de.dfki.vsm.xtension.yallah;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import io.javalin.Javalin;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class YallahExecutor extends ActivityExecutor implements ExportableProperties {

    /** The properties of ths plugin. */
    private final YallahProperties mYallahProperties = new YallahProperties() ;

    /** The websocket server reading messages coming from the YALLAH visualizer. */
    private Javalin mWebSocketServer;

    /** The connection instance. Not null when a connection os established with a remote client. */
    private WsConnectContext mSocketConnectCtx = null;

    /** The incremental counter used as unique ID for the sentences to speak, and as thread identifier. */
    private int mSentenceCounter = 0 ;

    /** The map of activity workers. This is used to keep track of threads waiting for the sentences to be spoken on the client.
     * The key is a unique identifier of the spoken text.
     * The value is the reference to the thread waiting inside the `execute()` method.
     */
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();


    public YallahExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
    }

    private final static String MARKER = "$" ;

    @Override
    public String marker(long id)
    {
        return MARKER+id;
    }


    @Override
    public void launch() {
        mLogger.message("YALLAH plugin launching...");

        //
        // Setup the WebSocket server to get messages from a YALLAH application.
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));
        mLogger.message("Starting the WebSocket server on port " + port + "...");

        mWebSocketServer = Javalin.create().start(port);

        // This sequence sets up what the websocket server must do when events occur
        mWebSocketServer.ws("/", ws -> {

            // Client Connection
            ws.onConnect(ctx -> {
                mLogger.message("Getting connection request.");
                mSocketConnectCtx = ctx;
            });

            // Client Message
            ws.onMessage(this::handleMessage);

            // Client disconnection
            ws.onClose(ctx -> {
                mSocketConnectCtx = null;

                mLogger.message("Socket closed");

                mLogger.message("Remove active (but not needed anymore) activity actions");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }
            });

            // Server error
            ws.onError(ctx -> {
                mLogger.failure("Error handling ws message exchange:" + ctx) ;
                mLogger.message("Remove active activity actions to avoid deadlocks...");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }

            });
        });


        //
        //
        String launch_mode = mConfig.getProperty("launchMode") ;
        YallahProperties.LaunchMode lm = YallahProperties.LaunchMode.valueOf(launch_mode) ;
        switch (lm) {
            case None:
                break ;
            case App:
                mLogger.failure("Launch mode " + lm.toString() + "not supported yet. YALLAH not launched.");
                break;
            case WebPage:
                mLogger.failure("Launch mode " + lm.toString() + "not supported yet. YALLAH not launched.");
                break;
            default:
                mLogger.warning("Unrecognized launch method " + lm.toString() );
        }


        //
        mLogger.message("YALLAH plugin launched.");
    }

    @Override
    public void unload() {
        mLogger.message("YALLAH plugin unloading....");

        // Shutdown the WebSocket server
        mWebSocketServer.stop();
        mSocketConnectCtx = null ;

        mLogger.message("YALLAH plugin unloaded.");
    }


    @Override
    public void execute(AbstractActivity activity) {

        mLogger.message("YALLAH Agent '" + activity.getActor() + "' said: " + activity.getText());
        mLogger.message("Activity name: " + activity.getName() + ", type: " + activity.getType() + ", features: " + activity.getFeatures());

        if (activity instanceof SpeechActivity) {
            //
            // SPEECH activity
            SpeechActivity sa = (SpeechActivity) activity;

            String punct = sa.getPunct() ;
            String text_only = sa.getTextOnly(MARKER).trim() ;
            LinkedList<String> time_marks = sa.getTimeMarks(MARKER);

            mLogger.message("This is a Speech Activity. text only: '" + text_only + "'; punct: '" + punct + "'"
                    + "There are " + time_marks.size() + " time marks") ;
            time_marks.forEach(mLogger::message);

            if (text_only.isEmpty()) {
                //
                // If text is empty, there is no need to send the marker to the client:
                // execute them all of them immediately.

                LinkedList<String> timemarks = sa.getTimeMarks(YallahExecutor.MARKER);
                for (String tm : timemarks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }

            } else {

                //
                // Send the sentence to the client
                String text = activity.getText();

                if (mSocketConnectCtx != null) {

                    String json_string = "{\n"
                            + "\"type\": \"text\",\n"
                            + "\"text\": \"" + text + "\",\n"
                            + "\"id\": \"" + mSentenceCounter + "\"\n"
                            + "}";

                    mSocketConnectCtx.send(json_string);

                    // Wait for client answer
                    synchronized (mActivityWorkerMap) {
                        // organize wait for feedback if (activity instanceof SpeechActivity) {
                        ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                        mActivityWorkerMap.put(mSentenceCounter + "", cAW);

                        // wait until we get notified
                        while (mActivityWorkerMap.containsValue(cAW)) {
                            try {
                                mActivityWorkerMap.wait();
                            } catch (InterruptedException exc) {
                                mLogger.failure(exc.toString());
                            }
                        }

                        // This must stay in the synch block.
                        mSentenceCounter++;

                    }
                }
            }

        } else {
            //
            // It is an [ACTION (with features)]
            // aka [COMMAND (with parameters)]

            String cmd = activity.getName() ;
            final LinkedList<ActionFeature> features = activity.getFeatures();

            String json_string = "{\n"
                    + "\"type\": \"command\",\n"
                    + "\"command\": \"" + cmd + "\",\n"
                    + "\"parameters\": \"" ;

            // The Action "features" are actually the parameters.
            for (int i = 0, featuresSize = features.size(); i < featuresSize; i++) {
                ActionFeature af = features.get(i);

                json_string += af.getKey() + "=" + af.getVal() ;
                // Add a comma only if it is not the last element
                if (i < featuresSize-1) json_string += "," ;
            }

            json_string += "\"\n"
                    + "}\n" ;

            if(mSocketConnectCtx != null) {
                mSocketConnectCtx.send(json_string);
            }

        }

    }


    /** Invoked every time the WebSocket server receives a message */
    private void handleMessage(@NotNull WsMessageContext wsMessageContext) {
        String message = wsMessageContext.message();

        if(message.startsWith("$")) {
            //
            // This is received when the client encountered a marker while speaking.

            mLogger.message("YALLAH: Tell VSM activity scheduler to handle action represented by time marker >" + message + "<");
            if (mProject.getRunTimePlayer().getActivityScheduler().hasMarker(message)) {
                mProject.getRunTimePlayer().getActivityScheduler().handle(message);
            } else {
                mLogger.failure("Marker has already been processed!");
            }
        } else if(message.startsWith("@")) {
            //
            // The client has finished speaking the sentence
            String text_id = message.substring(1) ;
            mLogger.message("Sentence " + text_id + " finished");

            // Remove the entry from the map and notify the threads to check again.
            synchronized (mActivityWorkerMap) {
                if (mActivityWorkerMap.containsKey(text_id)) {
                    //mLogger.message("Removing id from active activities ids ...");
                    mActivityWorkerMap.remove(text_id);
                }
                mActivityWorkerMap.notifyAll();
            }

        } else {
            mLogger.warning("Received unknown message format from the client '" + message + "'. Ignoring.");
        }

    }


    //
    // Plugin Properties
    //

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return mYallahProperties.getExportableProperties();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return mYallahProperties.getExportableAgentProperties();
    }
}
