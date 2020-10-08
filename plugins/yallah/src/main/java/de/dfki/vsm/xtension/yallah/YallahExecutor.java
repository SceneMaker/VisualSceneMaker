package de.dfki.vsm.xtension.yallah;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class YallahExecutor extends ActivityExecutor implements ExportableProperties {

    // The properties
    private final YallahProperties mYallahProperties = new YallahProperties() ;

    // The websocket server reading messages coming from the YALLAH visualizer
    private Javalin mWebSocketServer;

    // ??
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();


    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();


    public YallahExecutor(PluginConfig config, RunTimeProject project)
    {
        super(config, project);
    }

    @Override
    public String marker(long id)
    {
        return "$"+id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        mLogger.message("YALLAH Agent '" + activity.getActor() + "' said: " + activity.getText());

        mLogger.message("Activity type: " + activity.getType() + ", features: " + activity.getFeatures());

        //activity.getSubstitutions().forEach((k, v) -> subs += k + ", " + v + "; ");
        if(activity.getSubstitutions() != null) {
            String subs = "";
            for (Map.Entry<String, String> e : activity.getSubstitutions().entrySet()) {
                String k = e.getKey();
                String v = e.getValue();
                subs += subs += k + ", " + v + "; ";
            }
            mLogger.message(subs);
        } else {
            mLogger.message("No substitutions");
        }

    }

    @Override
    public void launch() {
        mLogger.message("YALLAH plugin launching...");

        //
        // Setup the WebSocket server to get messages from the YALLAH application.
        final int port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));
        mLogger.message("Launching the WebSocket server on port " + port + "...");

        //mWebSocketServer = Javalin.create(config -> config.enforceSsl = true).start(port);
        mWebSocketServer = Javalin.create().start(port);
        // mWebSocketServer.ws("/ws", ws -> {
        mWebSocketServer.ws("/", ws -> {

            // Client Connection
            ws.onConnect(ctx -> {
                mLogger.message("Getting connection request.");
                this.addWs(ctx);
            });

            // Client Message
            ws.onMessage(this::handleMessage);

            // Client disconnection
            ws.onClose(ctx -> {
                this.removeWs(ctx);

                mLogger.message("Socket closed");

                mLogger.message("Remove active (but not needed anymore) activity actions");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }
            });

            // Server error
            ws.onError(ctx -> mLogger.failure("Error handling ws message exchange:" + ctx));
        });


        //
        //
        String launch_mode = mConfig.getProperty("launchMode") ;
        YallahProperties.LaunchMode lm = YallahProperties.LaunchMode.valueOf(launch_mode) ;
        switch (lm) {
            case None:
                break ;
            case App:
                // TODO -- launch a standalone app
                mLogger.failure("Launch mode " + lm.toString() + "not supported yet. YALLAH not launched.");
                break;
            case WebPage:
                // TODO -- run an internal web server that provides the webpages and open them in a browser.
                mLogger.failure("Launch mode " + lm.toString() + "not supported yet. YALLAH not launched.");
        }


        //
        mLogger.message("YALLAH launched.");
    }

    @Override
    public void unload() {
        mLogger.message("YALLAH unloading....");

        //
        // TODO -- 1. Stop the YALLAH application (Not much to do if it is a WebPage)

        // 2. Shutdown the WebSocket server
        mWebSocketServer.stop();

        websockets.clear();

        mLogger.message("YALLAH unloaded.");
    }


    /** Invoked every time the WebSocket server receives a message */
    private void handleMessage(@NotNull WsMessageContext wsMessageContext) {
        mLogger.message("YALLAH: received websocket message: "+wsMessageContext.message());

        // Answer
        for (WsContext ws : websockets) {
            ws.send("Hi, too!");
        }

    }

    private synchronized void removeWs(@NotNull WsCloseContext ctx) {
        this.websockets.remove(ctx);
    }

    private synchronized void addWs(@NotNull WsConnectContext ws) {
        this.websockets.add(ws);
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
