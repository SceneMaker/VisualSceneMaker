package de.dfki.vsm.util.stickman;

import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.vsm.model.project.PluginConfig;

public class StickmanRepository {
    private final PluginConfig config;
    private StickmanAbstractFactory factory;
    public StickmanRepository(PluginConfig config) {
        this.config = config;
    }

    public StageRoom createStickman() {
        if (config.getProperty("stickman")!= null && config.getProperty("stickman").equals("StickmanLegacy")) {
            factory = new StickmanFactory(config);

        } else if(config.getProperty("stickman")!= null && config.getProperty("stickman").equals("Pinocchio")){
            factory = new Stickman3DFactory(config);
        } else {
            factory = new StickmanFxFactory(config);
        }
        return factory.getStickman();
    }

    public Animation getAnimation(String actor){
        return factory.getAnimation(actor);
    }

    public Animation loadEventAnimation(Stickman sm, String name, int duration, boolean block){
        return factory.loadEventAnimation(sm, name, duration, block);
    }
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block){
        return factory.loadAnimation(sm, name, duration, block);
    }
}