package de.dfki.vsm.util.stickman;

import de.dfki.common.decorators.StageRoomFullScreenDecorator;
import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.stickman3D.animationlogic.AnimationLoader3D;
import de.dfki.stickman3D.decorators.StageRoomNetwork3DDecorator;
import de.dfki.stickman3D.stage.StageRoom3D;
import de.dfki.vsm.model.project.PluginConfig;

import java.util.HashMap;

/**
 * Created by alvaro on 11/18/16.
 */
public class Stickman3DFactory extends StickmanAbstractFactory {
    public Stickman3DFactory(PluginConfig config) {
        super(config);
    }

    @Override
    public StageRoom getStickman() {
        final String host = config.getProperty("smhost");
        final String port = config.getProperty("smport");

        createInitialStageController();

        if (config.containsKey("fullscreen") && config.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
            stickmanStageC = new StageRoomFullScreenDecorator(stickmanStageC);
        }
        if(host !=null && port !=null && !host.equals("") && !port.equals("")){
            stickmanStageC =  new StageRoomNetwork3DDecorator(stickmanStageC, host, Integer.parseInt(port)) ;
        }
        return stickmanStageC;
    }

    private void createInitialStageController() {
        final String xPos = config.getProperty("xStage");
        final String yPos = config.getProperty("yStage");

        if (xPos !=null && yPos !=null) {
            try {
                stickmanStageC = new StageRoom3D(Integer.parseInt(xPos), Integer.parseInt(yPos));
            }catch (Exception e){
                stickmanStageC = new StageRoom3D();
            }

        }else{
            stickmanStageC = new StageRoom3D();
        }
    }

    @Override
    public Animation getAnimation(String actor) {
        return (Animation) AnimationLoader3D.getInstance();
    }

    @Override
    public Animation loadEventAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoader3D.getInstance().loadEventAnimation(sm, name, duration, false);
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoader3D.getInstance().loadAnimation(sm, name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block, HashMap<String, String> extraParams) {
        return AnimationLoader3D.getInstance().loadAnimation(sm, name, duration, block, extraParams);
    }
}
