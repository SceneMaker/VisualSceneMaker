package de.dfki.vsm.util.stickman;

import de.dfki.common.CommonAnimation;
import de.dfki.common.CommonStickman;
import de.dfki.common.StageStickmanController;
import de.dfki.vsm.model.project.PluginConfig;

/**
 * Created by alvaro on 9/19/16.
 */
public abstract class StickmanAbstractFactory {
    protected PluginConfig config;
    protected StageStickmanController stickmanStageC;
    protected abstract StageStickmanController getStickman();

    public abstract CommonAnimation getAnimation(String actor);
    public abstract CommonAnimation loadEventAnimation(CommonStickman sm, String name, int duration, boolean block);
    public abstract CommonAnimation loadAnimation(CommonStickman sm, String name, int duration, boolean block);

    public StickmanAbstractFactory(PluginConfig config) {
        this.config = config;
    }


}
