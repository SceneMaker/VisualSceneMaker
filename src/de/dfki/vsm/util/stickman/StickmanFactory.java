package de.dfki.vsm.util.stickman;

import de.dfki.common.agent.IAgent;
import de.dfki.common.animationlogic.IAnimation;
import de.dfki.common.decorators.StageRoomFullScreenDecorator;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.stickmanFX.animationlogic.AnimationLoaderFX;
import de.dfki.stickmanFX.decorators.StageRoomNetworkFXDecorator;
import de.dfki.stickmanFX.stage.StageRoomFX;
import de.dfki.vsm.model.project.PluginConfig;

import java.util.HashMap;

/**
 * Created by alvaro on 9/19/16.
 */
public class StickmanFactory extends StickmanAbstractFactory {
    public StickmanFactory(PluginConfig config) {
        super(config);
    }

    @Override
    protected StageRoom getStickman() {
        final String host = config.getProperty("smhost");
        final String port = config.getProperty("smport");
        StageRoom stage = new StageRoomFX();
        stickmanStageC = stage;
        if (config.containsKey("fullscreen") && config.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
            stickmanStageC = new StageRoomFullScreenDecorator(stage);
        }
        if(host !=null && port !=null && !host.equals("") && !port.equals("")){
            stickmanStageC =  new StageRoomNetworkFXDecorator(stickmanStageC, host, Integer.parseInt(port)) ;
        }
        return stickmanStageC;
    }

    @Override
    public IAnimation getAnimation(String actor) {
        return (IAnimation) AnimationLoaderFX.getInstance();
    }

    @Override
    public IAnimation loadEventAnimation(IAgent sm, String name, int duration, boolean block) {
        return AnimationLoaderFX.getInstance().loadEventAnimation(sm, name, duration, false);
    }

    @Override
    public IAnimation loadAnimation(IAgent sm, String name, int duration, boolean block) {
        return AnimationLoaderFX.getInstance().loadAnimation(sm, name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
    }

    @Override
    public IAnimation loadAnimation(IAgent sm, String name, int duration, boolean block, HashMap<String, String> extraParams) {
        return null;
    }
}
