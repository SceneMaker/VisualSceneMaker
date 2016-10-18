package de.dfki.vsm.util.stickman;

import de.dfki.common.CommonAnimation;
import de.dfki.common.CommonStickman;
import de.dfki.common.StageStickmanController;
import de.dfki.vsm.model.project.PluginConfig;

public class StickmanRepository {
    private final PluginConfig config;
    private StickmanAbstractFactory factory;
    public StickmanRepository(PluginConfig config) {
        this.config = config;
    }

    public StageStickmanController createStickman() {
        if (config.getProperty("stickman")!= null && config.getProperty("stickman").equals("StickmanLegacy")) {
            factory = new StickmanFactory(config);

        } else {
            factory = new StickmanFxFactory(config);
        }
        return factory.getStickman();
    }

    public CommonAnimation getAnimation(String actor){
        return factory.getAnimation(actor);
    }

    public CommonAnimation loadEventAnimation(CommonStickman sm, String name, int duration, boolean block){
        return factory.loadEventAnimation(sm, name, duration, block);
    }
    public CommonAnimation loadAnimation(CommonStickman sm, String name, int duration, boolean block){
        return factory.loadAnimation(sm, name, duration, block);
    }
}