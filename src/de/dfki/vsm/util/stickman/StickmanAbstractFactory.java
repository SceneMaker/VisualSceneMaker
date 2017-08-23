package de.dfki.vsm.util.stickman;

import de.dfki.common.agent.IAgent;
import de.dfki.common.animationlogic.IAnimation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.vsm.model.project.PluginConfig;

import java.util.HashMap;

/**
 * Created by alvaro on 9/19/16.
 */
public abstract class StickmanAbstractFactory {
    protected PluginConfig config;
    protected StageRoom stickmanStageC;
    protected abstract StageRoom getStickman();

    public abstract IAnimation getAnimation(String actor);
    public abstract IAnimation loadEventAnimation(IAgent sm, String name, int duration, boolean block);
    public abstract IAnimation loadAnimation(IAgent sm, String name, int duration, boolean block);
    public abstract IAnimation loadAnimation(IAgent sm, String name, int duration, boolean block, HashMap<String, String> extraParams);

    public StickmanAbstractFactory(PluginConfig config) {
        this.config = config;
    }


}
