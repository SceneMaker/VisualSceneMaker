/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.responsiveweb;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import io.javalin.Javalin;
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
    private final static String svalueSeparatorChar = "#";
    private final static String sCmdSeperatorChar = "$";

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
                String value = activity.get("value");
                if (value != null) {
                    value = value.replace("'", "");
                } else {
                    value = "";
                }
                broadcast(element + sCmdSeperatorChar + value);
            } else if (name.equalsIgnoreCase("setMoodGraph") ||
                    name.equalsIgnoreCase("setWorkHrsGraph")) {
                String element = activity.get("element");
                String day = activity.get("day");
                String type = activity.get("type");
                String value = activity.get("value");
                if (value != null) {
                    value = value.replace("'", "");
                } else {
                    value = "";
                }
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + day + svalueSeparatorChar +
                        type + svalueSeparatorChar + value);
            } else if (name.equalsIgnoreCase("setSpeechBubble")) {
                String element = activity.get("element");
                String producer = activity.get("producer");
                String value = activity.get("value");
                if (value != null) {
                    value = value.replace("'", "");
                } else {
                    value = "";
                }
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + producer + svalueSeparatorChar + value);
            } else if (name.equalsIgnoreCase("setMenuItem")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String id = activity.get("id");
                String value = activity.get("value").replace("'", "");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id + svalueSeparatorChar + value);
            } else if (name.equalsIgnoreCase("showElement")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String id = activity.get("id");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id);
            } else if (name.equalsIgnoreCase("hideElement")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String id = activity.get("id");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id);
            } else if (name.equalsIgnoreCase("muteMic")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                broadcast(element + sCmdSeperatorChar + name);
            } else if (name.equalsIgnoreCase("openMic")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                broadcast(element + sCmdSeperatorChar + name);
            } else if (name.equalsIgnoreCase("setAudioItem")) {
                String element = activity.get("element");
                String audio_src = "./" + activity.get("audio").replace("'", "");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + audio_src);
            } else if (name.equalsIgnoreCase("controlAudio")) {
                String element = activity.get("element");
                String control_type = activity.get("type");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + control_type);
            } else if (name.equalsIgnoreCase("stop")) {
                app.stop();
            } else if (name.equalsIgnoreCase("guiToFront")) {
                broadcast(name);
            } else if (name.equalsIgnoreCase("vcToFront")) {
                broadcast(name);
            } else if (!name.isEmpty()) { //check if name represents a webpage - must be configured in the device's agent as key, value pair.
                String guipage = mProject.getAgentConfig(activity_actor).getProperty(name);
                // send only if there is a stored html page
                if (guipage != null)
                    if (guipage.contains(".html")) {
                        broadcast(guipage);
                    }
            } else {
                mLogger.failure("Either agent " + activity_actor + " or content " + name + " is not valid.");
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
        final String audioFiles = (mProject.getProjectPath() + File.separator + mConfig.getProperty("audiofiles")).replace("\\", "/");
        final String sceneflowStateVar = mConfig.getProperty("sceneflowStateVar");
        mSceneflowInfoVar = mConfig.getProperty("sceneflowInfoVar");
        mPathToCertificate = mConfig.getProperty("certificate");

        if (mPathToCertificate != null) {
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
                    config.addStaticFiles(audioFiles, Location.EXTERNAL);
                    return server;
                });
            }).start();
        } else {
            app = Javalin.create(config -> {
                config.server(() -> {
                    Server server = new Server();
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    ServerConnector htmlConnector = new ServerConnector(server);
                    htmlConnector.setPort(html_port);
                    server.setConnectors(new Connector[]{connector, htmlConnector});
                    config.addStaticFiles(guiFiles, Location.EXTERNAL);
                    config.addStaticFiles(audioFiles, Location.EXTERNAL);
                    return server;
                });
            }).start();
        }

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

        // PG - Comment: This should be handled by a Sceneflow model!
//        if (message.equals("stopwatch")) {
//            broadcast("./audio_gui.html"); //arbeitszeit
//        }
//        else if (message.equals("calendar")) {
//            broadcast("./ui_stimmungsbarometer.html"); //moodgraph
//        } else if (message.equals("phone")) {
//            broadcast("./slider_gui.html"); //slider
//        } else if (message.equals("chat")) {
//            broadcast("./emotion_gui.html"); //emotion
//        } else if (message.equals("persons")) {
//            broadcast("./days_gui.html"); // days
//        } else if (message.equals("day_Montag")) {
//            broadcast("./conv_gui.html"); //conv
//        } else if (message.equals("home")) { //default
//            broadcast("./index.html");
//        } else if (message.equals("person")) {
//            broadcast(("./username_gui.html"));
//        } else if (message.equals("diary")) {
//            broadcast(("./conv_gui.html"));
//        }
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
}
