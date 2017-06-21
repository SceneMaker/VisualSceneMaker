package de.dfki.vsm.util.stickman;

import de.dfki.common.decorators.StageRoomFullScreenDecorator;
import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.stickmanSwing.animationlogic.AnimationLoaderSwing;
import de.dfki.stickmanSwing.decorators.StageRoomNetworkSwingDecorator;
import de.dfki.stickmanSwing.stage.StageRoomSwing;
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
    public StageRoom getStickman() {
        final String host = config.getProperty("smhost");
        final String port = config.getProperty("smport");
        StageRoom stage = new StageRoomSwing();
        stickmanStageC = stage;
        if (config.containsKey("fullscreen") && config.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
            stickmanStageC = new StageRoomFullScreenDecorator(stage);
        }
        if(host !=null && port !=null && !host.equals("") && !port.equals("")){
            stickmanStageC =  new StageRoomNetworkSwingDecorator(stickmanStageC, host, Integer.parseInt(port)) ;
        }
        return stickmanStageC;
    }

    @Override
    public Animation getAnimation(String actor) {
        return (Animation) AnimationLoaderSwing.getInstance();
    }

    @Override
    public Animation loadEventAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoaderSwing.getInstance().loadEventAnimation(sm, name, duration, false);
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoaderSwing.getInstance().loadAnimation(sm, name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block, HashMap<String, String> extraParams) {
        return null;
    }
}
