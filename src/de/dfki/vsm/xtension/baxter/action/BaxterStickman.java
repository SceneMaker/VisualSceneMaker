package de.dfki.vsm.xtension.baxter.action;

import com.objectplanet.image.PngEncoder;
import de.dfki.action.sequence.WordTimeMarkSequence;
import de.dfki.common.interfaces.Animation;
import de.dfki.common.interfaces.StageRoom;
import de.dfki.common.interfaces.Stickman;
import de.dfki.stickmanFX.animationlogic.AnimationLoaderFX;
import de.dfki.stickmanFX.animationlogic.EventAnimationFX;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.stickman.StickmanAbstractFactory;
import de.dfki.vsm.util.stickman.StickmanFxFactory;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.baxter.command.BaxterCommand;
import sun.misc.BASE64Encoder;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by alvaro on 6/1/16.
 */
public class BaxterStickman {

    private StickmanAbstractFactory factory ;
    StageRoom baxterStage;
    private final String name = "Baxter";
    Stickman mBaxterStickman;
    //final private Stickman mBaxterStickman = new Stickman("Baxter", Stickman.TYPE.MALE, 5.0f, new Dimension(1024, 600), false);

    public  BaxterStickman(PluginConfig mConfig){
        factory  = new StickmanFxFactory(mConfig);
        baxterStage = factory.getStickman();
        baxterStage.addStickman(name, true);
        mBaxterStickman = baxterStage.getStickman(name);
        baxterStage.launchStickmanStage(false);
        try {
            baxterStage.getStageAsImage();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String getImageHeadFromStickmanAnimation() {
        BufferedImage image = null;
        String headAsString = "";
        try {
            image = baxterStage.getStageAsImage();
            headAsString = transformStickmanToImage(image);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return headAsString;
    }

    private String transformStickmanToImage(BufferedImage head) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        String imageString = "";
        try {
            long startTime = System.currentTimeMillis();
            PngEncoder encoderPng = new PngEncoder();
            encoderPng.encode(head, baos);
            byte[] imageBytes = baos.toByteArray();

            BASE64Encoder encoder = new BASE64Encoder();
            imageString = encoder.encode(imageBytes);
            baos.close();
            long stopTime = System.currentTimeMillis();
            long elapsedTime = stopTime - startTime;
            System.out.println("**********************************************************"+elapsedTime+"********************************");

        } catch (IOException e) {
            e.printStackTrace();
        }
        return imageString;
    }

    public String buildBaxterCommand(ArrayList<String> params) {
        BaxterCommand command = new BaxterCommand("paint", "testId", params);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(command, iosw);
        String toSend = new String(out.toByteArray());
        toSend += "#END\n";
        return toSend;
    }



    public String getAnimationImage() {
        return getImageHeadFromStickmanAnimation();
    }

    public void loadNonBlockingAnimation(String animationName, int animationDuration){
        Animation animation = factory.loadAnimation(mBaxterStickman, animationName, animationDuration, false);
        mBaxterStickman.doAnimation(animationName, animationDuration, false);
    }

    public void loadBlockingAnimation(String animationName, int animationDuration){
        Animation animation = factory.loadAnimation(mBaxterStickman, animationName, animationDuration, true);
        mBaxterStickman.doAnimation(animationName, animationDuration, true);
    }

    public void loadBlockingAnimation(String animationName, int animationDuration, WordTimeMarkSequence wts){
        Animation animation = factory.loadAnimation(mBaxterStickman, animationName, animationDuration, true);
        mBaxterStickman.doAnimation(animationName, animationDuration, wts, true);
    }

    public Animation loadEventAnimation(String animationName, int animationDuration, WordTimeMarkSequence wts){
        Animation stickmanAnimation = AnimationLoaderFX.getInstance().loadEventAnimation(mBaxterStickman, animationName, animationDuration, false);
        stickmanAnimation.setParameter( wts);
        //loadNonBlockingAnimation(animationName, animationDuration);
        ((EventAnimationFX)stickmanAnimation).playEventAnimationPart();
        return stickmanAnimation;
        //mBaxterStickman.doEventFeedbackAnimation(animationName, animationDuration, wts, false);
        //mBaxterStickman.doAnimation(animationName, animationDuration, wts, false);
    }



}
