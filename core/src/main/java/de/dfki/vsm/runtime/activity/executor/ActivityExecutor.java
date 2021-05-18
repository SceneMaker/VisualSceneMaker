package de.dfki.vsm.runtime.activity.executor;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.scheduler.ActivityScheduler;
import de.dfki.vsm.runtime.player.RunTimePlayer;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.util.List;

/**
 * @author Gregor Mehlmann
 */
public abstract class ActivityExecutor extends RunTimePlugin {

    // The runtime player
    protected final RunTimePlayer mPlayer;
    // The activity scheduler
    protected final ActivityScheduler mScheduler;

    public ActivityExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        // Initialize runtime player
        mPlayer = mProject.getRunTimePlayer();
        // Initialize activity scheduler
        if(mPlayer != null)
            mScheduler = mPlayer.getActivityScheduler();
        else mScheduler = new ActivityScheduler();

    }

    public abstract String marker(final long id);

    public abstract void execute(final AbstractActivity activity);

    /** Support method to extract the value of a feature from the features dictionary.
     *
     * @param name
     * @param features
     * @return Empty string if the feature can't be found.
     */
    protected static String getActionFeatureValue(String name, List<ActionFeature> features) {
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("");
    }
}
