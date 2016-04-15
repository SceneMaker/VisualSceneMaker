/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.alma;

import de.affect.manage.AffectManager;
import de.affect.manage.event.AffectUpdateEvent;
import de.affect.manage.event.AffectUpdateListener;
import de.affect.util.AppraisalTag;
import de.affect.xml.AffectInputDocument;
import de.affect.xml.AffectOutputDocument;
import de.affect.xml.EmotionType;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.xmlbeans.XmlException;

/**
 *
 * @author Patrick Gebhard
 */
public class ALMAExecutor extends ActivityExecutor implements AffectUpdateListener {

    // The ALMA component
    AffectManager mALMA;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ALMAExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {

        final String name = activity.getName();

        AffectInputDocument.AffectInput ai = AppraisalTag.instance().makeAffectInput("User", name, "1.0", "Scene");
        mLogger.message("Processing " + ai.toString());
        mALMA.processSignal(ai);
    }

    @Override
    public void launch() {
        mLogger.message("Loading ALMA Regulated");
        // read config
        String sALMACOMP = mProject.getProjectPath() + File.separator + mConfig.getProperty("computation");
        sALMACOMP = sALMACOMP.replace("\\", "/");
        mLogger.message("Computation " + sALMACOMP);

        String sALMADEF = mProject.getProjectPath() + File.separator + mConfig.getProperty("definition");
        sALMADEF = sALMADEF.replace("\\", "/");
        mLogger.message("Definition " + sALMACOMP);

        try {
            mALMA = new AffectManager(sALMACOMP, sALMADEF, true);
        } catch (IOException | XmlException ex) {
            mLogger.failure("Unable to load ALMA Regulated. ALMA Regulated not available.");
            mLogger.failure(ex.getMessage());
        }
        mALMA.addAffectUpdateListener(this);
    }

    @Override
    public void unload() {
        //
    }

    @Override
    public void update(AffectUpdateEvent event) {
        AffectOutputDocument aod = event.getUpdate();

        try {
            for (Iterator<AffectOutputDocument.AffectOutput.CharacterAffect> it = aod.getAffectOutput().getCharacterAffectList().iterator(); it.hasNext();) {
                AffectOutputDocument.AffectOutput.CharacterAffect character = it.next();

                // access cached data or create new cache
                String name = character.getName();
                String emotion = character.getDominantEmotion().getName().toString();
                double eIntensity = Double.parseDouble(character.getDominantEmotion().getValue());
                String mood = character.getMood().getMoodword().toString();
                String mIntensity = character.getMood().getIntensity().toString();
                String mTendency = character.getMoodTendency().getMoodword().toString();

                // get the intensity of all active emotions of the character
                for (Iterator<EmotionType> emo = character.getEmotions().getEmotionList().iterator(); emo.hasNext();) {
                    EmotionType et = emo.next();
                }
            }
        } catch (Exception e) {
            mLogger.failure("Exception during affect update");
        }
    }
}
