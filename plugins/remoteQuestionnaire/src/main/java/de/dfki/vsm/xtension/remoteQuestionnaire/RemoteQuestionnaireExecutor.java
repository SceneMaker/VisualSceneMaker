package de.dfki.vsm.xtension.remoteQuestionnaire;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import io.javalin.Javalin;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsMessageContext;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Objects;


public class RemoteQuestionnaireExecutor extends ActivityExecutor implements ExportableProperties {

    private final static String MARKER = "$" ;

    /** The properties of ths plugin. */
    private final RemQuestProperties mRemQuestProperties = new RemQuestProperties() ;

    /** The websocket server listening for incoming connections from the questionnaire answerer. */
    private Javalin mWebSocketServer;

    /** The connection instance. Not null when a connection os established with a remote client. */
    private WsConnectContext mSocketConnectCtx = null;

    /** The incremental counter used as unique ID for the sentences to speak, and as thread identifier. */
    private int mQuestionCounter = 0 ;


    /** The map of activity workers. This is used to keep track of threads waiting for a
     * question to be answered by a client.
     * The key is a unique identifier of the question.
     * The value is the reference to the thread waiting inside the `execute()` method.
     */
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();


    public RemoteQuestionnaireExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return MARKER;
    }

    @Override
    public void launch() {
        // Start the http server
        mLogger.message("Questionnaire plugin launching...");

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


        // TODO -- setup a REST service to get answers from the avatar

    }

    @Override
    public void unload() {
        mLogger.message("Questionnaire plugin unloading....");

        // Shutdown the WebSocket server
        mWebSocketServer.stop();
        mSocketConnectCtx = null ;

        mLogger.message("Questionnaire plugin unloaded.");
    }



    @Override
    public void execute(AbstractActivity activity) {

        mLogger.message("Agent '" + activity.getActor() + "' said: " + activity.getText());
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
                // execute all the actions immediately.

                LinkedList<String> timemarks = sa.getTimeMarks(MARKER);
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
                            + "\"id\": \"" + mQuestionCounter + "\"\n"
                            + "}";

                    mSocketConnectCtx.send(json_string);

                    // Wait for client answer
                    synchronized (mActivityWorkerMap) {
                        // organize wait for feedback if (activity instanceof SpeechActivity) {
                        ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                        mActivityWorkerMap.put(mQuestionCounter + "", cAW);

                        // wait until we get notified
                        while (mActivityWorkerMap.containsValue(cAW)) {
                            try {
                                mActivityWorkerMap.wait();
                            } catch (InterruptedException exc) {
                                mLogger.failure(exc.toString());
                            }
                        }

                        // This must stay in the synch block.
                        mQuestionCounter++;

                    }
                }
            }

        } else {
            //
            // It is an [ACTION (with features)]
            // aka [COMMAND (with parameters)]

            String cmd = activity.getName() ;
            final LinkedList<ActionFeature> features = activity.getFeatures();

            StringBuilder json_string = new StringBuilder("{\n"
                    + "\"type\": \"command\",\n"
                    + "\"command\": \"" + cmd + "\",\n"
                    + "\"parameters\": \"");

            // The Action "features" are actually the parameters.
            for (int i = 0, featuresSize = features.size(); i < featuresSize; i++) {
                ActionFeature af = features.get(i);

                json_string.append(af.getKey()).append("=").append(af.getValNoQuotes());
                // Add a comma only if it is not the last element
                if (i < featuresSize-1) json_string.append(",");
            }

            json_string.append("\"\n" + "}\n");

            if(mSocketConnectCtx != null) {
                mSocketConnectCtx.send(json_string.toString());
            }

        }

    }



    private void handleMessage(@NotNull WsMessageContext wsMessageContext) {
        // Messages from a client are answers to questions
        // Get the question ID, and the answer, and log into our file.
    }


        //
    // Plugin Properties
    //

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return mRemQuestProperties.getExportableProperties();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return mRemQuestProperties.getExportableAgentProperties();
    }

}
