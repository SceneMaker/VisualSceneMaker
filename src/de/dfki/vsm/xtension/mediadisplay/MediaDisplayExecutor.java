/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.mediadisplay;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.manager.ActivityScheduler;
import de.dfki.vsm.runtime.activity.manager.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.HashSet;
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
    public void execute(AbstractActivity activity, ActivityScheduler player) {
        if (activity instanceof ActionActivity) {
            final String name = activity.getName();

            if (name.equalsIgnoreCase("show")) {
                mMediaDisplayGUI.setVisible(true);
            }

            if (name.equalsIgnoreCase("hide")) {
                mMediaDisplayGUI.setVisible(false);
            }
        }
    }

    @Override
    public void launch() {
        mMediaDisplayGUI = new MediaDisplayGUI();
        SwingUtilities.invokeLater(() -> mMediaDisplayGUI.init(this));
    }

    @Override
    public void unload() {
        // nothing
    }
}
