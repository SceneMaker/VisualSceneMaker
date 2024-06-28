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
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.xmlbeans.XmlException;
//import org.apache.xmlbeans.XmlException;

import java.io.*;
import java.util.LinkedList;
import java.util.*;

import static de.affect.emotion.EmotionType.Shame;

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

            //variable used if ALMA should automatically update the VCM variables off all Characters
            String sAutoUpdate = mConfig.getProperty("autoUpdate");
            if(sAutoUpdate.equals("true")){
                mALMA.addAffectUpdateListener(this);
                mLogger.message("ALMA autoUpdate enabled, alma will frequently update the Characters variables in VCM");
            }else {
                mLogger.message("ALMA autoUpdate disabled, alma will NOT frequently update the Characters variables in VCM. Updates only happen when the UpdateVars command iss called in VCM");
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
                executeAffectInputList(activity);
                return;
            }
            //case for padList
            if(name.equalsIgnoreCase("PADInput")){
                executePADInputList(activity);
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
        FloatValue P = new FloatValue((float) character.getCurrentMood().getPleasure());
        FloatValue A = new FloatValue((float) character.getCurrentMood().getArousal());
        FloatValue D = new FloatValue((float) character.getCurrentMood().getDominance());

        float[] pad = new float[]{(float) character.getCurrentMood().getPleasure(), (float) character.getCurrentMood().getArousal(), (float) character.getCurrentMood().getDominance()};
        String dominatnEmotion = getEmotions(pad);

        if (dominatnEmotion == "UNDEFINED"){
            return;
        }

        String curEmotionList = (String) mProject.getValueOf("TeacherEmotionList").getValue();

        curEmotionList = curEmotionList  + dominatnEmotion + ";";
        //mLogger.message(curEmotionList);


        //assign values to according variables in VCM
        mProject.setVariable(name_P,P );
        mProject.setVariable(name_A,A );
        mProject.setVariable(name_D,D );
        mProject.setVariable("TeacherEmotionList", curEmotionList);


        logToFile("TeacherPADtoEmotion.txt", "Teacher emotion: " + dominatnEmotion + " <---   " +P.getValue() + " : " + A.getValue()+ " : " + D.getValue() );
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

    //Process a PADInputList
    //called from within VCM by PlayAction ( "[CHARACKERNAME PADList list=LISTNAME]" )
    //in VCM there should be a string variable defined with the same name as in the action call
    //The string can be defined in the following ways(note n o space):
    //1. "P,A,D;P,A,D;......"
    //2. "P,A,D,INTESNITY;P,A,D,INTESNITY;P,A,D,INTESNITY;P,A,D,INTESNITY;P,A,D,INTESNITY;....."
    //3. "P,A,D,INTESNITY,ELICITOR;P,A,D,ELICITOR,INTESNITY,ELICITOR;P,A,D,ELICITOR,INTESNITY,ELICITOR;....."
    //the function the creates a corresponding AffectInput and sends it to ALMA
    //Default values for intensity is 1 and for elicitor "Scene", all unelegantly set within the code
    private void executePADInputList(AbstractActivity activity) {

        //load vsm variable(name defined by list) contents into local variable appraisal_list_s
        String pad_list_name = getActionFeatureValue("list", activity.getFeatures());
        String pad_list_s = (String) mProject.getValueOf(pad_list_name).getValue();

        mProject.setVariable(pad_list_name, "");
        mLogger.message("PAD list with list string: " + pad_list_s + " on actor " + activity.getActor());

        if (pad_list_s == "") {
            return;
        }

        //split string by ; to create list and loop over all the emlements
        List<String> appraisal_list = new ArrayList<>(Arrays.asList(pad_list_s.split(";")));
        for (String affectString : appraisal_list) {
            String[] affect = affectString.split(",");

            //logToFile(affect[0]);
            if(!(affect[0].equals("0.0") && affect[1].equals("0.0") && affect[2].equals("0.0"))){
                AffectInputDocument.AffectInput ai;
                switch (affect.length) {
                    case 5:
                        ai = AppraisalTag.instance().makePADInput(activity.getActor(),affect[0],affect[1],affect[2], affect[3], affect[4]);
                        mLogger.message("Processing " + ai.toString());
                        mALMA.processSignal(ai);
                        break;
                    case 4:
                        ai = AppraisalTag.instance().makePADInput(activity.getActor(),affect[0],affect[1],affect[2],  affect[3], "Bob");
                        mLogger.message("Processing " + ai.toString());
                        mALMA.processSignal(ai);
                        break;
                    case 3:
                        String intensity = "0.8";//getPADInputIntensity(affect[0],affect[1],affect[2]);

                        float[] pad = {Float.parseFloat(affect[0]),Float.parseFloat(affect[1]),Float.parseFloat(affect[2])};
                        String padEmotion = getEmotions(pad);
                        logToFile("AffectInput.txt", padEmotion + " <---  Intsity: " + intensity +   "     P:A:D " +affect[0] + " : " + affect[1]+ " : " +affect[2] );

                        ai = AppraisalTag.instance().makePADInput(activity.getActor(),affect[0],affect[1],affect[2], intensity, "Bob");
                        //mLogger.message("Processing " + ai.toString());
                        mALMA.processSignal(ai);
                        break;
                    default:
                        mLogger.failure("Partially Incorrect Appraisal List format in: " + affect);
                }
            }
        }
    }

    private String getPADInputIntensity(String sP , String sA, String sD) {
        //these emotions get an intesity of 1
        //TODO keep this list updated
        String[] priorityEmotionList = {"Fear","Hate","Anger","Shame","Joy","Disappointment","Distress","Sorry-For","Remorse"};
    
        float[] pad = {Float.parseFloat(sP),Float.parseFloat(sA),Float.parseFloat(sD)};
        String padEmotion = getEmotions(pad);


        for(String emotion : priorityEmotionList ){
            if(padEmotion.equals(emotion)){
                mLogger.message("For Emotion " + padEmotion + " return intesity 1");
                return "1.0";
            }
        }

        mLogger.message("For Emotion " + padEmotion + " return intesity 0,5");

        return "0.5";
    }

    //Process a AffectInputList
    //called from within VCM by PlayAction ( "[CHARACKERNAME AffectList list=LISTNAME]" ) example PlayAction("[AlmaTeacher AffectList list=affectList]")
    //in VCM there should be a string variable defined with the same name as in the action call
    //The string can be defined in the following ways:
    //1. "APPRAISALTAG;APPRAISALTAG;APPRAISALTAG;APPRAISALTAG;APPRAISALTAG;......"
    //2. "APPRAISALTAG,INTESNITY;APPRAISALTAG,INTESNITY;APPRAISALTAG,INTESNITY;APPRAISALTAG,INTESNITY;APPRAISALTAG,INTESNITY;....."
    //3. "APPRAISALTAG,INTESNITY,ELICITOR;APPRAISALTAG,ELICITOR,INTESNITY,ELICITOR;APPRAISALTAG,ELICITOR,INTESNITY,ELICITOR;....."
    //the function the creates a corresponding AffectInput and sends it to ALMA
    //Default values for intensity is 1 and for elicitor "Scene", all unelegantly set within the code
    private void executeAffectInputList(AbstractActivity activity) {
        //load vsm variable(name defined by list) contents into local variable appraisal_list_s
        String appraisal_list_name = getActionFeatureValue("list", activity.getFeatures());
        String appraisal_list_s = (String) mProject.getValueOf(appraisal_list_name).getValue();

        mProject.setVariable(appraisal_list_name, "");
        mLogger.message("AffectList with list string: " +appraisal_list_s + " on actor "+ activity.getActor());

        if(appraisal_list_s == ""){return;}//if list empty do nothing

        //split string by ; to create list and loop over all the emlements
        List<String> appraisal_list = new ArrayList<>(Arrays.asList(appraisal_list_s.split(";")));
        for (String affectString : appraisal_list) {
            String[] affect = affectString.split(",");

            //logToFile(affect[0]);
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
                    ai = AppraisalTag.instance().makeAffectInput(activity.getActor(), "GoodEvent", "1", "Scene");
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
    // the 4 variables should be named and initialised in the VCM accordingly for each character
    @Override
    public void update(AffectUpdateEvent event) {
        AffectOutputDocument aod = event.getUpdate();
         try {
            for (AffectOutputDocument.AffectOutput.CharacterAffect character : aod.getAffectOutput().getCharacterAffectList()) {
                mLogger.message("ALMA automatic update event");

                //logCharacterUpdate(character, true);

                //getting the PAD values from the character, casting them to VSM type and collect them to vars
                FloatValue P = new FloatValue((float) character.getMood().getPleasure());
                FloatValue A = new FloatValue((float) character.getMood().getArousal());
                FloatValue D = new FloatValue((float) character.getMood().getDominance());

                //update project vars accordingly
                String pName = character.getName()+"P";
                String aName = character.getName()+"A";
                String dName = character.getName()+"D";

                mProject.setVariable(pName,P );
                mProject.setVariable(aName,A );
                mProject.setVariable(dName,D );
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

    //used to determine the currently strongest emotion, using the current PAD value of the ALMA character and the cosign similarity to the possible emotions
    //note, this function is only necessary if P A D values are used as Input for ALMA
    //when appraisal-tags are used strongestEmotion() can be called on an alma character. (when PAD values are usd as inutp, this function will allways return Physical)
    //
    private String getEmotions(float[] padValues){
        List<List<String>> padToEm = readCSV("plugins/mithos/data/PAD_to_emotion.csv", ",");

        //we search the highest cosine similarity and emotion withs this:
        double max_similarity = -1.0;
        String cosestEmotion = "UNDEFINED";


        for (List<String> line : padToEm) {
            String emotion = (line.get(0));
            float emP = new Float(line.get(1));
            float emA = new Float(line.get(2));
            float emD = new Float(line.get(3));
            float[] emPAD = {emP,emA,emD};


            Double cosineSimilarity = cosineSimilarity(emPAD, padValues);

            if(cosineSimilarity >= max_similarity){
                max_similarity = cosineSimilarity;
                cosestEmotion = line.get(0);
            }
        }


        return cosestEmotion;
    }

    //used by getEmotion
    //function used to read the csv files containing the tables for the PAD to eomotion and Appraisal tag to emotion mapping
    private List<List<String>> readCSV(String csvFile, String csvSplitBy){
        String line = "";
        List<List<String>> csvData = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                // Split the line by comma
                List<String> data = new ArrayList<String>();
                String[] array = line.split(csvSplitBy);

                for (String s:array) {
                    data.add(s);
                }
                csvData.add(data);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        //enable logging of reading
        boolean print = false;
        if(print){
            String toPrint = "|";
            for (List<String> csvLine : csvData) {
                for (String s : csvLine) {
                    toPrint = toPrint + s + " | ";

                }
                toPrint = toPrint + "\n";
            }
            mLogger.message(toPrint);
        }
        return csvData;
    }

    // Function to calculate cosine similarity
    private double cosineSimilarity(float[] vectorA, float[] vectorB) {
        double dotProduct = 0;
        double magnitudeA = 0;
        double magnitudeB = 0;

        for (int i = 0; i < vectorA.length; i++) {
            dotProduct += vectorA[i] * vectorB[i];
            magnitudeA += Math.pow(vectorA[i], 2);
            magnitudeB += Math.pow(vectorB[i], 2);
        }

        magnitudeA = Math.sqrt(magnitudeA);
        magnitudeB = Math.sqrt(magnitudeB);

        if (magnitudeA == 0 || magnitudeB == 0) {
            return 0; // to handle division by zero
        }

        return (float)dotProduct / (magnitudeA * magnitudeB);
    }

///Some log functions
    //logs string to a file used for debugging, can be deleted
private static Boolean logToFile = false;
    //logs string to a file used for debugging, can be deleted
    public static void logToFile(String filename, String message) {
        if(logToFile){
            try(PrintWriter writer = new PrintWriter( new FileWriter( filename , true))) {
                writer.println(message);
            }
            catch
            (IOException e) {
                System.err.println("Error writing to log file: "+ e.getMessage());
            }
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
}
