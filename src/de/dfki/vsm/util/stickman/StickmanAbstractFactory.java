package de.dfki.vsm.util.stickman;

import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.vsm.model.project.PluginConfig;

import java.util.HashMap;

/**
 * Created by alvaro on 9/19/16.
 */
public abstract class StickmanAbstractFactory {
    protected PluginConfig config;
    protected StageRoom stickmanStageC;
    public abstract StageRoom getStickman();

    public abstract Animation getAnimation(String actor);
    public abstract Animation loadEventAnimation(Stickman sm, String name, int duration, boolean block);
    public abstract Animation loadAnimation(Stickman sm, String name, int duration, boolean block);
    public abstract Animation loadAnimation(Stickman sm, String name, int duration, boolean block, HashMap<String, String> extraParams);

    public StickmanAbstractFactory(PluginConfig config) {
        this.config = config;
    }


}
