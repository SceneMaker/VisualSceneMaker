package de.dfki.vsm.xtension.towerdefence;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.LinkedList;

/**
 *
 * @author Jan Stieling
 * 
 */
public class TowerDefenceExecutor extends ActivityExecutor {

    // The TowerDefence component
    TowerDefenceGUI mTowerDefence;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public TowerDefenceExecutor(PluginConfig config, RunTimeProject project) {
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

            if (name.equalsIgnoreCase("start")) {
                if (activity.getActor().equalsIgnoreCase("board")) {
                    mLogger.message("Start Enemies  ...");
                    
                    mTowerDefence.startGame();
                }
            }
            
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading Exclusive Tower Defence");
        
        if (mTowerDefence == null) {
            mTowerDefence = new TowerDefenceGUI(this);
        } else {
            mTowerDefence.setVisible(true);
        }
    }

    @Override
    public void unload() {
        mTowerDefence.setVisible(false);
    }

    // get the value of a feature (added PG) - quick and dirty
    private final String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature af : features) {
            if (af.getKey().equalsIgnoreCase(name)) {
                return af.getVal();
            }
        }
        return "";
    }

}
