package de.dfki.vsm.util.stickman;

import de.dfki.common.agent.IAgent;
import de.dfki.common.animationlogic.IAnimation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.vsm.model.project.PluginConfig;

import java.util.HashMap;

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
        } else if(config.getProperty("stickman")!= null && config.getProperty("stickman").equals("Reeti")){
            factory = new ReetiFactory(config);
        } else {
            factory = new StickmanFxFactory(config);
        }
        return factory.getStickman();
    }

    public IAnimation getAnimation(String actor){
        return factory.getAnimation(actor);
    }

    public IAnimation loadEventAnimation(IAgent sm, String name, int duration, boolean block){
        return factory.loadEventAnimation(sm, name, duration, block);
    }
    public IAnimation loadAnimation(IAgent sm, String name, int duration, boolean block){
        return factory.loadAnimation(sm, name, duration, block);
    }


    public IAnimation loadAnimation(IAgent stickman, String name, int duration, boolean block, HashMap<String, String> extraParams) {
        return factory.loadAnimation(stickman, name, duration, block, extraParams);
    }
}