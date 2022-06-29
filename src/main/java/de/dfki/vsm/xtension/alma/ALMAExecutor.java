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
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.xmlbeans.XmlException;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 *
 * 15.2.2022 - added pad interface for MindBot requirement to process p a d values as input
 * 26.6.2022 - added the vsm variable mechanism to write list of active emotions for every simulated agent
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

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String name = activity.getName();
            final LinkedList<ActionFeature> features = activity.getFeatures();

            if (name.equalsIgnoreCase("reset")) {
                mLogger.message("Reset affect processing for  " + activity.getActor());
                mALMA.resetCharacters();
            }

            // appraisal tag input
            if (AppraisalTag.instance().isAppraisalTag(name)) {
                String elicitor = getActionFeatureValue("elicitor", features);
                elicitor = (elicitor.isEmpty()) ? "Scene" : elicitor;

                AffectInputDocument.AffectInput ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), name, "1.0", elicitor);
                mLogger.message("Processing " + ai.toString());
                mALMA.processSignal(ai);
            }

            // pleasure, arousal, and dominance values as input (cf. MindBot)
            if (name.equalsIgnoreCase("pad")) {
                String elicitor = getActionFeatureValue("elicitor", features);
                elicitor = (elicitor.isEmpty()) ? "Situation" : elicitor;
                String pleasureStr = getActionFeatureValue("p", features);
                String arousalStr = getActionFeatureValue("a", features);
                String dominanceStr = getActionFeatureValue("d", features);

                AffectInputDocument.AffectInput ai = AppraisalTag.instance().makePADInput(activity.getActor(), pleasureStr, arousalStr, dominanceStr, "1.0", elicitor);
                mLogger.message("Processing " + ai.toString());
                mALMA.processSignal(ai);
            }
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading ALMA - 2022 (PAD)");
        if (mALMA == null) {
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
                mLogger.failure("Unable to load ALMA - 2022 (PAD)");
                mLogger.failure(ex.getMessage());
            }
            mALMA.addAffectUpdateListener(this);
        } else {
            mALMA.startRealtimeOutput(mALMA.getDocumentManager().getAffectComputationParams());
        }
        
        //mALMA.stepwiseAffectComputation();
    }

    @Override
    public void unload() {
        mALMA.stopAll();
    }

    @Override
    public void update(AffectUpdateEvent event) {
        AffectOutputDocument aod = event.getUpdate();

        try {
            for (AffectOutputDocument.AffectOutput.CharacterAffect character : aod.getAffectOutput().getCharacterAffectList()) {
                // access cached data or create new cache
                String name = character.getName();
                String emotion = character.getDominantEmotion().getName().toString();
                double eIntensity = Double.parseDouble(character.getDominantEmotion().getValue());
                String mood = character.getMood().getMoodword().toString();
                String mIntensity = character.getMood().getIntensity().toString();
                String mTendency = character.getMoodTendency().getMoodword().toString();

                LinkedList<AbstractValue> valueList = new LinkedList<>();

                // get the intensity of all active emotions of the user
                for (EmotionType et : character.getEmotions().getEmotionList()) {
                    if (Float.parseFloat(et.getValue()) > 0.25f) {
                        StringValue sv = new StringValue(et.getName().toString());
                        valueList.add(sv);
                    }
                }

                try {
                    ListValue list = new ListValue(valueList);
                    String VSMEntityEmotionlistVar = name + "_emotions";

                    if (mProject.hasVariable(VSMEntityEmotionlistVar)) {
                        mProject.setVariable(VSMEntityEmotionlistVar, list);
                    } else {
                        mLogger.failure(VSMEntityEmotionlistVar + " is not available in model -> simulated emotions will not be updated");
                    }
                } catch (Exception e) {
                    // System.out.println("not running");
                }
            }
        } catch (Exception e) {
            mLogger.failure("Exception during affect update");
        }
    }
    // get the value of a feature (added PG) - quick and dirty

    private final String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature af : features) {
            if (af.getKey().equalsIgnoreCase(name)) {
                return af.getVal();
            }
        }
        return "";
    }

}
