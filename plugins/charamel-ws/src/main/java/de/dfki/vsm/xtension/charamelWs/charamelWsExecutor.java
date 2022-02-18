/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.charamelWs;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.util.communication.ReceiveSenderPort;
import de.dfki.vsm.xtension.util.communication.implementations.JavalinWebsocketBroadcaster;
import de.dfki.vsm.xtension.util.plugin.implementation.PluginAdapter;

import java.util.Objects;

/**
 * @author Lenny Händler, Patrick Gebhard
 */
public class charamelWsExecutor extends PluginAdapter {
    // PG: 18.11.2020 global Sceneflow variable for the whole turn information.

    private ReceiveSenderPort<String, String> receiveSenderPort;

    public charamelWsExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }


    @Override
    public void launch() {
        mLogger.message("Loading Charamel VuppetMaster Executor (WebSocket) ...");
        final int wss_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("wss_port")));
        final int ws_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("ws_port")));
        final String sceneflowVar = mConfig.getProperty("sceneflowVar");
        String VSMCharacterSpeakingVar = mConfig.getProperty("characterSpeaking");

        String mPathToCertificate = mConfig.getProperty("certificate");
        receiveSenderPort = new JavalinWebsocketBroadcaster<>(wss_port, mPathToCertificate, "/ws");
        receiveSenderPort.registerDisconnectHandler(this::purgeActivities);

        this.mPlugin = new Charamel(receiveSenderPort, VSMCharacterSpeakingVar, mLogger, this);
    }

    @Override
    public void unload() {
        receiveSenderPort.stop();
    }

}
