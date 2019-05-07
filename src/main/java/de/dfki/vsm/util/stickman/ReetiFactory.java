/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.util.stickman;

import de.dfki.common.decorators.StageRoomFullScreenDecorator;
import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.reeti.animationlogic.AnimationLoaderReeti;
import de.dfki.reeti.decorators.StageRoomNetwork3DDecorator;
import de.dfki.reeti.stage.StageRoomReeti;

import de.dfki.vsm.model.project.PluginConfig;
import java.util.HashMap;

/**
 *
 * @author EmpaT
 */
public class ReetiFactory extends StickmanAbstractFactory
{

    public ReetiFactory(PluginConfig config)
    {
        super(config);
    }

    @Override
    protected StageRoom getStickman()
    {
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
                stickmanStageC = new StageRoomReeti(Integer.parseInt(xPos), Integer.parseInt(yPos));
            }catch (Exception e){
                stickmanStageC = new StageRoomReeti();
            }

        }else{
            stickmanStageC = new StageRoomReeti();
        }
    }

    @Override
    public Animation getAnimation(String actor) {
        return (Animation) AnimationLoaderReeti.getInstance();
    }

    @Override
    public Animation loadEventAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoaderReeti.getInstance().loadEventAnimation(sm, name, duration, false);
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block) {
        return AnimationLoaderReeti.getInstance().loadAnimation(sm, name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
    }

    @Override
    public Animation loadAnimation(Stickman sm, String name, int duration, boolean block, HashMap<String, String> extraParams) {
        return AnimationLoaderReeti.getInstance().loadAnimation(sm, name, duration, block, extraParams);
    }
    
}
