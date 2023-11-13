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
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.xmlbeans.XmlException;
//import org.apache.xmlbeans.XmlException;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;

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
            if (AppraisalTag.instance().isAppraisalTag(name)) {
                String elcitor = getActionFeatureValue("elicitor", features);
                elcitor = (elcitor.isEmpty()) ? "Scene" : elcitor;

                AffectInputDocument.AffectInput ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), name, "1.0", elcitor);
                mLogger.message("Processing " + ai.toString());
                mALMA.processSignal(ai);
            }
        }
    }

    @Override
    public void launch() {
        mLogger.message("Loading ALMA Regulated");
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
                mLogger.failure("Unable to load ALMA Regulated. ALMA Regulated not available.");
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
                logCharacterUpdate(character, true);
                //TODO MABY AUTOMATICALLY ADD CHARACTER VARS TO VSM?


                StringValue dominatnEmotion = new StringValue(character.getDominantEmotion().getName().toString());

                //gettiong  PAP values from character, casting them to VSM type and collect them to vars
                FloatValue P = new FloatValue((float) character.getMood().getPleasure());
                FloatValue A = new FloatValue((float) character.getMood().getArousal());
                FloatValue D = new FloatValue((float) character.getMood().getDominance());


                //update project vars accordingly
                String emotionName = character.getName()+ "Emotion";
                String pName = character.getName()+"P";
                String aName = character.getName()+"A";
                String dName = character.getName()+"D";

                mProject.setVariable(emotionName,dominatnEmotion );
                mProject.setVariable(pName,P );
                mProject.setVariable(aName,A );
                mProject.setVariable(dName,D );


                LinkedList<AbstractValue> valueList = new LinkedList<>();

                // get the intensity of all active emotions of the character
                for (EmotionType et : character.getEmotions().getEmotionList()) {
                    if (Float.parseFloat(et.getValue()) > 0.25f) {
                        String sv = et.getName().toString();
                        valueList.add(new StringValue(sv));

                    }
                }
            }
        } catch (Exception e) {
            mLogger.failure("Exception during affect update");
        }
    }

    // print character attributes With Logger.
    // Priority level is set to message, if not shown, set LOG_LEVELs in enviroment to ALL
    //bool detailed is used to set the deteil level of the print
    private void logCharacterUpdate(AffectOutputDocument.AffectOutput.CharacterAffect character, boolean detailed){
        // access cached data or create new cache
        String name = character.getName();
        String emotion = character.getDominantEmotion().getName().toString();
        double eIntensity = Double.parseDouble(character.getDominantEmotion().getValue());
        String mood = character.getMood().getMoodword().toString();
        String mIntensity = character.getMood().getIntensity().toString();
        String mTendency = character.getMoodTendency().getMoodword().toString();

        //steing message for logger updatre message
        String updateInfo = ">>> Character Update\n";
        updateInfo +=   "  Character Name: "+ name + "\n";
        updateInfo +=   "  Strongest Emotion: " + emotion + " " +eIntensity + "\n";

        if(detailed){
            updateInfo +=   "  All Emotions:\n";
            for (EmotionType et : character.getEmotions().getEmotionList()) {
                updateInfo +=   "      " + et.getName() + " has value:" +et.getValue() + "\n";
            }
        }
        updateInfo +=   "  Mood: " + mIntensity+ " " + mood  + " with tendency towards " + mTendency + "\n";

        if(detailed){
            updateInfo +=   "    (P,A,D) = (" +character.getMood().getPleasure()+", " +character.getMood().getArousal()+ ", "+character.getMood().getDominance()+",)";
        }

        mLogger.message( updateInfo);
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
