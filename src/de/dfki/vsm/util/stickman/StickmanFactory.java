package de.dfki.vsm.util.stickman;

import de.dfki.common.CommonAnimation;
import de.dfki.common.CommonStickman;
import de.dfki.common.StageStickmanController;
import de.dfki.stickman.animationlogic.AnimationLoader;
import de.dfki.stickman.stagecontroller.StageController;
import de.dfki.stickman.stagecontroller.decorators.StageStickmanFullScreenDecorator;
import de.dfki.stickman.stagecontroller.decorators.StageStickmanNetworkControllerDecorator;
import de.dfki.vsm.model.project.PluginConfig;

/**
 * Created by alvaro on 9/19/16.
 */
public class StickmanFactory extends StickmanAbstractFactory {
    public StickmanFactory(PluginConfig config) {
        super(config);
    }

    @Override
    protected StageStickmanController getStickman() {
        final String host = config.getProperty("smhost");
        final String port = config.getProperty("smport");
        StageStickmanController stage = new StageController();
        stickmanStageC = stage;
        if (config.containsKey("fullscreen") && config.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
            stickmanStageC = new StageStickmanFullScreenDecorator(stage);
        }
        if(host !=null && port !=null && !host.equals("") && !port.equals("")){
            stickmanStageC =  new StageStickmanNetworkControllerDecorator(stickmanStageC, host, Integer.parseInt(port)) ;
        }
        return stickmanStageC;
    }

    @Override
    public CommonAnimation getAnimation(String actor) {
        return (CommonAnimation) AnimationLoader.getInstance();
    }

    @Override
    public CommonAnimation loadEventAnimation(CommonStickman sm, String name, int duration, boolean block) {
        return AnimationLoader.getInstance().loadEventAnimation(sm, name, duration, false);
    }

    @Override
    public CommonAnimation loadAnimation(CommonStickman sm, String name, int duration, boolean block) {
        return AnimationLoader.getInstance().loadAnimation(sm, name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
    }
}
