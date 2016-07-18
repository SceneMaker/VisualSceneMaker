/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ReceiverExecutor extends ActivityExecutor {

    ReceiverThread mMessagereceiver;

    private String mSceneflowVar;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ReceiverExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String name = activity.getName();
            final LinkedList<ActionFeature> features = activity.getFeatureList();

            if (name.equalsIgnoreCase("stop")) {
                mMessagereceiver.stopServer();
            }
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading Message Receiver");
        final int port = Integer.parseInt(mConfig.getProperty("port"));

        mSceneflowVar = mConfig.getProperty("sceneflow_variable");

        mMessagereceiver = new ReceiverThread(this, port);
        mMessagereceiver.start();
    }

    @Override
    public void unload() {
        mMessagereceiver.stopServer();
    }

    public boolean hasProjectVar(String var) {
        return mProject.hasVariable(var);
    }
    
    public void setSceneFlowVariable(String message) {
        mLogger.message("Assigning sceneflow variable " + mSceneflowVar + " with value " + message);
        mProject.setVariable(mSceneflowVar, new StringValue(message));
    }

    public void setSceneFlowVariable(String var, String value) {
        mLogger.message("Assigning sceneflow variable " + var + " with value " + value);
        mProject.setVariable(var, new StringValue(value));
    }

//    // get the value of a feature (added PG) - quick and dirty
//    private final String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
//        for (ActionFeature af : features) {
//            if (af.getKey().equalsIgnoreCase(name)) {
//                return af.getVal();
//            }
//        }
//        return "";
//    }
}
