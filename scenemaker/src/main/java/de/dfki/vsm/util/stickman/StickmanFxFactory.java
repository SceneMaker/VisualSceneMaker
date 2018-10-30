package de.dfki.vsm.util.stickman;

import de.dfki.common.decorators.StageRoomFullScreenDecorator;
import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.stickmanFX.animationlogic.AnimationLoaderFX;
import de.dfki.stickmanFX.decorators.StageRoomNetworkFXDecorator;
import de.dfki.stickmanFX.stage.StageRoomFX;
import de.dfki.vsm.model.project.PluginConfig;
import java.io.File;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by alvaro on 9/19/16.
 */
public class StickmanFxFactory extends StickmanAbstractFactory {

    public StickmanFxFactory(PluginConfig config) {
        super(config);
    }

    @Override
    protected StageRoom getStickman() {
        final String host = config.getProperty("smhost");
        final String port = config.getProperty("smport");

        createInitialStageController();

        if (config.containsKey("fullscreen") && config.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
            stickmanStageC = new StageRoomFullScreenDecorator(stickmanStageC);
        }
        if (host != null && port != null && !host.equals("") && !port.equals("")) {
            stickmanStageC = new StageRoomNetworkFXDecorator(stickmanStageC, host, Integer.parseInt(port));
        }
        return stickmanStageC;
    }

    private void createInitialStageController() {
        final String xPos = config.getProperty("xStage");
        final String yPos = config.getProperty("yStage");

        if (xPos != null && yPos != null) {
            try {
                stickmanStageC = new StageRoomFX(Integer.parseInt(xPos), Integer.parseInt(yPos));
            } catch (Exception e) {
                stickmanStageC = new StageRoomFX();
            }

        } else {
            stickmanStageC = new StageRoomFX();
        }
    }

    @Override
    public Animation getAnimation(String actor) {
        return (Animation) AnimationLoaderFX.getInstance();
    }

    @Override
    public Animation loadEventAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoaderFX.getInstance().loadEventAnimation(sm, name, duration, false);
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoaderFX.getInstance().loadAnimation(sm, name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block, HashMap<String, String> extraParams) {
        Animation a = AnimationLoaderFX.getInstance().loadAnimation(sm, name, duration, false);
        String paranater = "";
        for (Map.Entry<String, String> entry : extraParams.entrySet()) {
            if (!entry.getValue().isEmpty()) {
                if (isFloat(entry.getValue())) {
                    a.setParameter(entry.getValue().replace("'", ""));
                } else if (entry.getValue().contains(".")) {
                    String filepath = "res" + File.separator + "background";
                    String fileAbsolutePath = new File(filepath).getAbsolutePath();
                    String sfileAbsolutePath = fileAbsolutePath + File.separator + entry.getValue().replace("'", "");
                    a.setParameter(sfileAbsolutePath);
                } else {
                    a.setParameter(entry.getValue().replace("'", ""));
                }
            }
        }
        return a;
    }

    private boolean isFloat(String input) {
        try {
            Float.parseFloat(input);
            return true;
        } catch (Exception e) {
            return false;
        }
    }
}
