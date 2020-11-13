/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.responsiveweb;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import io.javalin.Javalin;
import io.javalin.core.JavalinConfig;
import io.javalin.http.staticfiles.Location;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.util.ssl.SslContextFactory;

import java.io.File;
import java.util.*;

/**
 * @author Lenny Händler, Patrick Gebhard
 */
public class HtmlGuiWsExecutor extends ActivityExecutor {
    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    private Javalin app;
    private String mPathToCertificate = "";
    private String mSceneflowInfoVar = "";

    public HtmlGuiWsExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "${'" + id + "'}$";
    }

    @Override
    public void execute(AbstractActivity activity) {
        final String activity_actor = activity.getActor();

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("${'").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("${'");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                mLogger.warning("No gui command with CMD markers send ...");
            }
        } else {
            final String name = activity.getName();
            //final LinkedList<ActionFeature> features = activity.getFeatures();

            if (name.equalsIgnoreCase("set")) {
                String element = activity.get("element");
                String value = activity.get("value").replace("'", "");
                broadcast(element + ":" + value);
            } else if (name.equalsIgnoreCase("setMoodGraph")) {
                String cmd = name;
                String element = activity.get("element");
                String day = activity.get("day");
                String type = activity.get("type");
                String value = activity.get("value").replace("'", "");
                broadcast(element + ":" + cmd + "_" + day + "_" + type + "_" + value);
            } else if (name.equalsIgnoreCase("setWorkHrsGraph")) {
                String cmd = name;
                String element = activity.get("element");
                String day = activity.get("day");
                String type = activity.get("type");
                String value = activity.get("value").replace("'", "");
                broadcast(element + ":" + cmd + "_" + day + "_" + type + "_" + value);
            } else if (name.equalsIgnoreCase("stop")) {
                app.stop();
            } else if (!name.isEmpty()) { //check if name represents a webpage - must be configured in the device's agent as key, value pair.
                String guipage = mProject.getAgentConfig(activity_actor).getProperty(name);
                // send only if there is a stored html page
                if (guipage.contains(".html")) {
                    broadcast(guipage);
                }
            }
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading HTML GUI Executor (WebSocket) ...");
        final int wss_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("wss_port")));
        final int ws_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("ws_port")));
        final int html_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("html_port")));
        final String guiFiles = (mProject.getProjectPath() + File.separator + mConfig.getProperty("guifiles")).replace("\\", "/");
        final String sceneflowStateVar = mConfig.getProperty("sceneflowStateVar");
        mSceneflowInfoVar = mConfig.getProperty("sceneflowInfoVar");
        mPathToCertificate = mConfig.getProperty("certificate");

        app = Javalin.create(config -> {
            config.server(() -> {
                Server server = new Server();
                ServerConnector sslConnector = null;
                sslConnector = new ServerConnector(server, getSslContextFactory());
                sslConnector.setPort(wss_port);
                ServerConnector connector = new ServerConnector(server);
                connector.setPort(ws_port);
                ServerConnector htmlConnector = new ServerConnector(server);
                htmlConnector.setPort(html_port);
                server.setConnectors(new Connector[]{sslConnector, connector, htmlConnector});
                config.addStaticFiles(guiFiles, Location.EXTERNAL);
                return server;
            });
        }).start();
        app.get("/", ctx -> {
            ctx.redirect("/index.html");
        });
        app.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                this.addWs(ctx);
                mLogger.message("Connected to Browser");
                // test
                //broadcast("./default_gui.html");
                // let sceneflow know that a client has connected
                if (mProject.hasVariable(sceneflowStateVar)) {
                    mProject.setVariable(sceneflowStateVar, true);
                }
            });
            ws.onMessage(this::handleMessage);
            ws.onClose(ctx -> {
                this.removeWs(ctx);

                mLogger.message("Closed");
                mLogger.message("Remove active (but not needed anymore) activity actions");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }
            });
            ws.onError(ctx -> mLogger.failure("Error handling ws message exchange"));
        });
    }

    private synchronized void handleMessage(WsMessageContext ctx) {
        String message = ctx.message();
        mLogger.message("Processing Browser GUI message: >" + message + "<");

        // let sceneflow know that a client has send a message.
        if (mProject.hasVariable(mSceneflowInfoVar)) {
            mProject.setVariable(mSceneflowInfoVar, message);
        }
        if (message.equals("stopwatch")) {
            broadcast("./ui_arbeitszeit.html");
        }

        else if (message.equals("calendar")) {
            broadcast("./ui_stimmungsbarometer.html");
        }

        else if (message.equals("home")) {
            broadcast("./index.html");
        }
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        this.websockets.add(ws);
    }

    private synchronized void broadcast(String msg) {
        for (WsContext ws : websockets) {
            ws.send(msg);
        }
    }

    @Override
    public void unload() {
        websockets.clear();
        app.stop();
    }

    private SslContextFactory getSslContextFactory() {
        SslContextFactory sslContextFactory = new SslContextFactory.Server();
        sslContextFactory.setKeyStorePath(this.getClass().getResource(mPathToCertificate).toExternalForm()); //default "/my-release-key.keystore"
        sslContextFactory.setKeyStorePassword("123456");
        return sslContextFactory;
    }

    // get the value of a feature (added PG) - quick and dirty
    private String getActionFeatureValue(String name, List<ActionFeature> features) {
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("");
    }
}
