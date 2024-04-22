/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.alma;

import de.affect.manage.AffectManager;
import de.affect.manage.CharacterManager;
import de.affect.manage.event.AffectUpdateEvent;
import de.affect.manage.event.AffectUpdateListener;
import de.affect.util.AppraisalTag;
import de.affect.xml.AffectInputDocument;
import de.affect.xml.AffectOutputDocument;
import de.affect.xml.EmotionType;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.PauseActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.xmlbeans.XmlException;
//import org.apache.xmlbeans.XmlException;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.*;

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

            Boolean toggleAffectUpdate = Boolean.parseBoolean(mConfig.getProperty("affectUpdate"));
            if(toggleAffectUpdate){
                mALMA.addAffectUpdateListener(this);
            }

        } else {
            mALMA.startRealtimeOutput(mALMA.getDocumentManager().getAffectComputationParams());
        }

        //mALMA.stepwiseAffectComputation();
    }

    //Method called by VSM on the ALMA VCM agent. This function mostly delegates to the corresponding subfunction to a given activity
    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            executeSpeechActivity((SpeechActivity) activity);
            return;
        }

        if (activity instanceof PauseActivity)
        {
            //TODO
            mLogger.message("Pause was called, NOT IMPLEMENTED");
        }

        if (activity instanceof ActionActivity) {
            final String name = activity.getName();

            if (name.equalsIgnoreCase("reset")) {
                mLogger.message("Reset affect processing for  " + activity.getActor());
                mALMA.resetCharacters();
                return;
            }

            if(name.equalsIgnoreCase("UpdateVars")){
                mLogger.message("Update Vars was called");

                executeUpdateVars(activity);
            }

            //case for appraisalTagList
            if(name.equalsIgnoreCase("AffectList")){
                executeAffectList(activity);
                return;
            }
            if (AppraisalTag.instance().isAppraisalTag(name)) {
                executeAppraisalTag(activity);
                return;
            }
        }
    }

    //given the names for P, A, D and strongest emotion Variables, update them accordingly
    private void executeUpdateVars(AbstractActivity activity) {
        //find alma characte belonging to activity
        CharacterManager character = mALMA.getCharacterByName(activity.getActor().toString());

        //get he names of the vars in the vcm project from action input
        String name_P = getActionFeatureValue("P", activity.getFeatures()).replace("'", "");
        String name_A = getActionFeatureValue("A", activity.getFeatures()).replace("'", "");
        String name_D = getActionFeatureValue("D", activity.getFeatures()).replace("'", "");
        String name_emotion = getActionFeatureValue("emotion", activity.getFeatures()).replace("'", "");

        //get valjues from alma
        StringValue dominatnEmotion = new StringValue(character.getCurrentEmotions().getDominantEmotion().toString());
        FloatValue P = new FloatValue((float) character.getCurrentMood().getPleasure());
        FloatValue A = new FloatValue((float) character.getCurrentMood().getArousal());
        FloatValue D = new FloatValue((float) character.getCurrentMood().getDominance());

        //assign values to according variables in VCM
        mProject.setVariable(name_P,P );
        mProject.setVariable(name_A,A );
        mProject.setVariable(name_D,D );
        mProject.setVariable(name_emotion,dominatnEmotion );
    }

    private void executeAppraisalTag(AbstractActivity activity) {
        final LinkedList<ActionFeature> features = activity.getFeatures();

        String elcitor = getActionFeatureValue("elicitor", features);
        elcitor = (elcitor.isEmpty()) ? "Scene" : elcitor;

        String intensity = getActionFeatureValue("intensity", features);
        intensity = (intensity.isEmpty()) ? "1" : intensity;

        AffectInputDocument.AffectInput ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), activity.getName(), intensity, elcitor);
        mLogger.message("Processing " + ai.toString());
        mALMA.processSignal(ai);
    }

    private void executeAffectList(AbstractActivity activity) {
        String appraisal_list_name = getActionFeatureValue("list", activity.getFeatures());
        String appraisal_list_s = (String) mProject.getValueOf(appraisal_list_name).getValue();
        mProject.setVariable(appraisal_list_name, "");
        mLogger.message("AffectList with list string: " +appraisal_list_s + " on actor "+ activity.getActor());
        if(appraisal_list_s == ""){return;}
        List<String> appraisal_list = new ArrayList<>(Arrays.asList(appraisal_list_s.split(";")));

        for (String affectString : appraisal_list) {
            String[] affect = affectString.split(",");

            AffectInputDocument.AffectInput ai;
            switch (affect.length) {
                case 3:
                    ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), affect[0], affect[1], affect[2]);
                    mLogger.message("Processing " + ai.toString());
                    mALMA.processSignal(ai);
                    break;
                case 2:
                    ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), affect[0], affect[1], "Scene");
                    mLogger.message("Processing " + ai.toString());
                    mALMA.processSignal(ai);
                    break;
                case 1:
                    ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), affect[0], "1", "Scene");
                    mLogger.message("Processing " + ai.toString());
                    mALMA.processSignal(ai);
                    break;
                default:
                    mLogger.failure("Partially Incorrect Appraisal List format in: " + affect);
            }
        }


    }

    //this method handles the execution of speech activities
    private void executeSpeechActivity(SpeechActivity activity) {
        SpeechActivity sa = activity;
        String text = sa.getTextOnly("$(").trim();
        LinkedList<String> timemarks = sa.getTimeMarks("$(");

        // If text is empty - assume activity has empty text but has marker activities registered
        if (text.isEmpty()) {
            for (String tm : timemarks) {
                mLogger.warning("Directly executing activity at timemark " + tm);
                mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
            }
        }
    }

    @Override
    public void unload() {
        mALMA.stopAll();
    }

    //this function is called by ALMA each time an affect update is happening
    //this function updates for each character the corresponding VSM variables
    // the 4 variables should be named and initialised in the VCM accordingly for each caracter
    @Override
    public void update(AffectUpdateEvent event) {
        AffectOutputDocument aod = event.getUpdate();
         try {
            for (AffectOutputDocument.AffectOutput.CharacterAffect character : aod.getAffectOutput().getCharacterAffectList()) {
                //logCharacterUpdate(character, true);
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
