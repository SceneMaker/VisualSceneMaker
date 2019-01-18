/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.reeti;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;

/**
 *
 * @author Robbie
 */
public class SSIUtility extends SSIRunTimePlugin {

    // The flag if we use the JPL
    private final boolean mUseSSI;
    // The flag for executables
//    private final boolean mUseExe;
    private static SSIUtility sInstance = null;

    public SSIUtility(
            final PluginConfig config,
            final RunTimeProject project,
            boolean useSSI) {
        super(config, project);
        // Get the JPL flag value
//        mUseJPL = Boolean.parseBoolean(
//                mConfig.getProperty("usejpl"));
        mUseSSI = useSSI;
        sInstance = this;
        // Get the executable flag value
//        mUseExe = Boolean.parseBoolean(
//                mConfig.getProperty("useexe"));
    }

    // Launch SSI plugin
    @Override
    public void launch() {
        if (mUseSSI) {
            super.launch();
            JPLEngine.load("swi_reeti/logic.pl");
        }
    }

    // Unload SSI plugin
    @Override
    public void unload() {
        if (mUseSSI) {
            super.unload();

            // Print some information 
            mLogger.message("Unloading SSI");
        }
    }

    @Override
    public void handle(final SSIEventArray array) {
        // Print some information 
        //mLogger.message("Handling SSI event array:\n " + array.toString());
        for (final SSIEventEntry event : array.list()) {
            final SSIEventData data = event.getData();
            if (event.getSender().equalsIgnoreCase("audio")
                    && event.getEvent().equalsIgnoreCase("vad")) {
                if (event.getState().equalsIgnoreCase("completed")) {
                    // User stopped speaking
                    mLogger.message("User stopped speaking");
                    if (mUseSSI) {
                        JPLEngine.query("now(Time), "
                                + "jdd(["
                                + "type:" + "event" + ","
                                + "mode:" + "voice" + ","
                                + "name:" + "user" + ","
                                + "time:" + "Time" + ","
                                + "from:" + event.getFrom() + ","
                                + "life:" + event.getDur() + ","
                                + "conf:" + event.getProb() + ","
                                + "data:" + "stop"
                                + "]).");
                    } else {
                        // Set speaking variable
                        mProject.setVariable("UserIsSpeaking", false);
                    }
                } else if (event.getState().equalsIgnoreCase("continued")) {
                    // User started speaking
                    mLogger.message("User started speaking");
                    if (mUseSSI) {
                        JPLEngine.query("now(Time), "
                                + "jdd(["
                                + "type:" + "event" + ","
                                + "mode:" + "voice" + ","
                                + "name:" + "user" + ","
                                + "time:" + "Time" + ","
                                + "from:" + event.getFrom() + ","
                                + "life:" + event.getDur() + ","
                                + "conf:" + event.getProb() + ","
                                + "data:" + "start"
                                + "]).");
                    } else {
                        // Set speaking variable
                        mProject.setVariable("UserIsSpeaking", true);
                    }
                } else {
                    // Should not happen
                }
            } else if (event.getSender().equalsIgnoreCase("speech")
                    && event.getEvent().equalsIgnoreCase("act")) {
                final String keyword = ((SSIStringData) data).toString().trim();
                // User started speaking
                mLogger.message("User said '" + keyword + "'");
                if (mUseSSI) {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + ","
                            + "mode:" + "speech" + ","
                            + "name:" + "user" + ","
                            + "time:" + "Time" + ","
                            + "from:" + event.getFrom() + ","
                            + "life:" + event.getDur() + ","
                            + "conf:" + event.getProb() + ","
                            + "data:" + "'" + keyword + "'"
                            + "]).");
                } else {
                    // Set keyword variable
                    mProject.setVariable("UserSaidKeyword", keyword);
                }
                // This condition is used to receive structure sent by SSI
            } else if (event.getSender().equalsIgnoreCase("audio")
                    && event.getEvent().equalsIgnoreCase("speech")) {
                final String structure = ((SSIStringData) data).toString().trim();
                // User started speaking
                mLogger.message("User said '" + structure + "'");
                if (mUseSSI) {
                    JPLEngine.query("now(Time), "
                            + "jdd(["
                            + "type:" + "event" + ","
                            + "mode:" + "speech" + ","
                            + "name:" + "user" + ","
                            + "time:" + "Time" + ","
                            + "from:" + event.getFrom() + ","
                            + "life:" + event.getDur() + ","
                            + "conf:" + event.getProb() + ","
                            + "data:" + structure
                            + "]).");
                }
            } else {
                // This should not happen
            }
        }
    }
    public static void switchGrammar(String gtammar) {
        if(sInstance.getSender() != null){
            sInstance.getSender().sendString(gtammar + '\000');
        }
    }
}
