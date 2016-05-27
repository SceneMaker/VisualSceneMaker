/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.buttongui;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.HashSet;
import java.util.LinkedList;
import javafx.application.Platform;

/**
 *
 * @author Patrick Gebhard
 */
public class ButtonGuiExecutor extends ActivityExecutor {

    // The GUI
    Gui mButtonGui = new Gui(this);;
    // The current ActivityWorker
    ActivityWorker mActivityWorker = null;
    private final HashSet<ActivityWorker> mActivityWorkers = new HashSet<>();
    // Configuration values
    public final HashSet<ButtonValues> mButtonsAndValues = new HashSet<>();
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ButtonGuiExecutor(PluginConfig config, RunTimeProject project) {
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

                mButtonGui.setVisible(true);
            }

            if (name.equalsIgnoreCase("hide")) {
                mLogger.warning("Hide button gui");
                mButtonGui.hideAllButtons();
                mButtonGui.setVisible(false);
            }
        }
    }

    public void setVSmVar(String var, String value) {
        mProject.setVariable(var, new StringValue(value));
    }

    @Override
    public void launch() {
        mLogger.message("Lauching ButtonGUI ...");
        mButtonGui.setVisible(false);
        // format for button config
        //<Feature key="button_yes" val="100, 100, 24, "Yes","yes_pressed", "user_input"/>
        for (ConfigFeature cf : mConfig.getEntryList()) {
            String key = cf.getKey();
            if (key.contains("button")) {
                String[] values = cf.getValue().split(",");

                ButtonValues bv = new ButtonValues(key,
                        Integer.parseInt(values[0].trim()),
                        Integer.parseInt(values[1].trim()),
                        Integer.parseInt(values[2].trim()),
                        values[3].trim(),
                        values[4].trim(),
                        values[5].trim());

                mButtonsAndValues.add(bv);
            }
        }
        Platform.runLater(() -> mButtonGui.initFX());
    }

    @Override
    public void unload() {
        mButtonGui.setVisible(false);
    }
}
