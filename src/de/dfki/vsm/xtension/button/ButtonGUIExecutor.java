/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.button;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import javafx.application.Platform;
import javax.swing.JOptionPane;

/**
 *
 * @author Patrick Gebhard
 */
public class ButtonGUIExecutor extends ActivityExecutor {

    // The GUI
    final ButtonGUI mButtonGui = new ButtonGUI(this);

    // The current ActivityWorker
    ActivityWorker mActivityWorker = null;
    private final HashSet<ActivityWorker> mActivityWorkers = new HashSet<>();
    // Configuration values
    public final HashSet<ButtonValues> mButtonsAndValues = new HashSet<>();
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ButtonGUIExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity/*, ActivityScheduler player*/) {
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

            if (name.equalsIgnoreCase("show")) {
                mLogger.warning("Show button gui");

                mButtonGui.hideAllButtons();

                String[] buttons = mProject.getAgentConfig(activity.getActor()).getProperty("show").split(",");
                for (String b : buttons) {
                    mButtonGui.showButton(b.trim(), true);
                }

                //mButtonGui.setVisible(true);
            }

            if (name.equalsIgnoreCase("hide")) {
                mLogger.warning("Hide button gui");
                mButtonGui.hideAllButtons();
                //mButtonGui.setVisible(false);
            }
        }
    }

    public void setVSmVar(String var, String value) {
        mProject.setVariable(var, new StringValue(value));
    }

    @Override
    public void launch() {
        mLogger.message("Lauching ButtonGUI ...");
        // format for button config
        // id, x, y, name, value, scenemaker var
        //<Feature key="button_yes" val="100, 100, 24, "Yes","yes_pressed", "user_input"/>
        if (!mButtonGui.isInitialized()) {
//            String missingVariables = "";
//            int missingVarCnt = 0;
//            ArrayList<VarDef> globalVars = mProject.getSceneFlow().getParentNode().getVarDefList();

            for (ConfigFeature cf : mConfig.getEntryList()) {
                String key = cf.getKey();

                if (key.equalsIgnoreCase("hideonpressed")) {
                    if (cf.getValue().equalsIgnoreCase("true") || cf.getValue().equalsIgnoreCase("false")) {
                        mButtonGui.mHideOnPressed = Boolean.parseBoolean(cf.getValue());
                    }
                }
                if (key.equalsIgnoreCase("alwaysontop")) {
                    if (cf.getValue().equalsIgnoreCase("true") || cf.getValue().equalsIgnoreCase("false")) {
                        mButtonGui.mAlwaysOnTop = Boolean.parseBoolean(cf.getValue());
                    }
                }
                if (key.equalsIgnoreCase("takesallinput")) {
                    if (cf.getValue().equalsIgnoreCase("true") || cf.getValue().equalsIgnoreCase("false")) {
                        mButtonGui.mModal = Boolean.parseBoolean(cf.getValue());
                    }
                }

                if (key.contains("button")) {
                    String[] values = cf.getValue().split(",");

                    ButtonValues bv = new ButtonValues(key,
                            Integer.parseInt(values[0].trim()),
                            Integer.parseInt(values[1].trim()),
                            Integer.parseInt(values[2].trim()),
                            values[3].trim(),
                            values[4].trim(),
                            values[5].trim());

//                    // Check if scene_flow has the global variable values[5].trim();
//                    mLogger.message("Checking var " + values[5].trim());
//
//                    boolean hasVar = false;
//                    for (VarDef vd : globalVars) {
//                        hasVar = (!hasVar) ? vd.getName().equals(values[5].trim()) : false;
//                        if (hasVar) break;
//                    }
//                    
//                    if (!hasVar) {
//                        missingVarCnt++;
//                        missingVariables += values[5].trim() + ",";
//                    }

                    mButtonsAndValues.add(bv);
                }
            }

//            // show dialog if global var is missing.
//            if (missingVarCnt > 0) {
//                missingVariables = missingVariables.substring(0, missingVariables.length() - 1);
//                JOptionPane.showMessageDialog(mButtonGui,
//                        "Please add global " + ((missingVarCnt > 1) ? " variables to sceneflow " + missingVariables : " variable to sceneflow " + missingVariables),
//                        "Variable not defined!",
//                        JOptionPane.WARNING_MESSAGE);
//            }

            Platform.runLater(() -> mButtonGui.initFX());
        }
    }

    @Override
    public void unload() {
        mButtonGui.hideAllButtons();
    }
}
