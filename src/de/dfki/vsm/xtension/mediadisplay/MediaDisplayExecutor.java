/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.mediadisplay;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import javax.swing.SwingUtilities;

/**
 *
 * @author Patrick Gebhard
 */
public class MediaDisplayExecutor extends ActivityExecutor {

    // The GUI
    MediaDisplayGUI mMediaDisplayGUI;
    // The current ActivityWorker
    ActivityWorker mActivityWorker = null;
    private final HashSet<ActivityWorker> mActivityWorkers = new HashSet<>();
    // Configuration values
    private final HashMap<String, String> mDisplayValues = new HashMap<>();
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public MediaDisplayExecutor(PluginConfig config, RunTimeProject project) {
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
            activity.setType(AbstractActivity.Type.parallel);

            final String name = activity.getName();

            if (name.equalsIgnoreCase("show")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("image")) {
                        mMediaDisplayGUI.setImage(af.getVal());
                    }
                    mMediaDisplayGUI.setVisible(true);
                }
            }

            if (name.equalsIgnoreCase("hide")) {
                mMediaDisplayGUI.setVisible(false);
            }
        }
    }

    @Override
    public void launch() {
        mMediaDisplayGUI = new MediaDisplayGUI();

        for (ConfigFeature cf : mConfig.getEntryList()) {
            mDisplayValues.put(cf.getKey(), cf.getValue());
        }
        mDisplayValues.put("path", mProject.getProjectPath());

        SwingUtilities.invokeLater(() -> mMediaDisplayGUI.init(this, mDisplayValues));
    }

    @Override
    public void unload() {
        // nothing
    }
}
