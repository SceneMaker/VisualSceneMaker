/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.stickmantts;

import de.dfki.action.sequence.WordTimeMarkSequence;

import de.dfki.stickmanfx.StickmanStageFX;
import de.dfki.stickmanfx.animationlogic.AnimationFX;
import de.dfki.stickmanfx.animationlogic.AnimationLoaderFX;
import de.dfki.util.xml.XMLUtilities;
import de.dfki.util.ios.IOSIndentWriter;
import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.tts.SpeakerTts;
import de.dfki.vsm.util.tts.TTSFactory;
import de.dfki.vsm.util.tts.marytts.MaryTTsProcess;

import de.dfki.vsm.util.tts.VoiceName;


import de.dfki.vsm.xtension.stickmanmarytts.action.ActionMouthActivity;
import de.dfki.vsm.xtension.stickmantts.util.tts.SpeakerActivity;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

import java.io.*;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 */
public class StickmanTtsExecutor extends ActivityExecutor {

    // The singelton logger instance
    private static StickmanStageFX mStickmanStage;
    private  Thread stickmanLaunchThread;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private StickmanTtsListener mListener;
    private final HashMap<String, StickmanTtsHandler> mClientMap = new HashMap();
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    private HashMap<String, String> languageAgentMap;
    private HashMap<String, SpeakerActivity> speechActivities = new HashMap<>();
    private HashMap<String, WordTimeMarkSequence> wtsMap= new HashMap<>();
    private MaryTTsProcess marySelfServer;
    public static String sExecutionId = "stickmanmary_";
    private String mDeviceName;

    private int maryId;

    // Construct the executor
    public StickmanTtsExecutor(final PluginConfig config, final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        maryId = 0;
        languageAgentMap = new HashMap<>();
        //marySelfServer = MaryTTsProcess.getsInstance(mConfig.getProperty("mary.base"));

    }

    public void accept(final Socket socket) {
        final StickmanTtsHandler client = new StickmanTtsHandler(socket, this);
        // TODO: Get some reasonable name for references here!
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        mLogger.warning("Accepting " + client.getName() + "");
    }

    @Override
    public final String marker(final long id) {
        // Stickman style bookmarks
        return "$" + id;
    }

    private final String getExecutionId() {
        return sExecutionId + maryId++;
    }

    @Override
    public void execute(AbstractActivity activity) {
        final String actor = activity.getActor();
        final String name = activity.getName();
        AgentConfig agent = mProject.getAgentConfig(actor);
        String langVoice = getLangVoiceFromConfig(actor);
        String voice = agent.getProperty(langVoice);
        VoiceName voiceName = new VoiceName(voice);
        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$").trim();
            if (text.isEmpty()) {
                handleEmptyTextActivity(sa);
            }
            //MaryTTsSpeaker marySpeak = new MaryTTsSpeaker(sa, langVoice, voiceName);
            TTSFactory factoryTTs = new TTSFactory(mConfig, sa, mProject);
            SpeakerTts speakerTts = factoryTTs.getTTs();
            SpeakerActivity speakerActivity = new SpeakerActivity(speakerTts);

            String executionId = getExecutionId();
            WordTimeMarkSequence wts = speakerActivity.getWordTimeSequence();
            //We will use these two later
            speechActivities.put(executionId, speakerActivity);
            wtsMap.put(executionId, wts);
            executeSpeachAndWait(executionId);
        } else if (activity instanceof ActionActivity || activity instanceof ActionMouthActivity) {
            if (name.equalsIgnoreCase("set") && activity instanceof ActionActivity) {
                actionSetVoice(activity, agent);
            } else {
                actionLoadAnimation(activity, actor, name);
            }
        }
    }

    private void actionLoadAnimation(AbstractActivity activity, String actor, String name) {
        AnimationFX stickmanAnimation = new AnimationFX();
        int duration = 500;
        if (activity instanceof ActionMouthActivity) {
            duration = ((ActionMouthActivity) activity).getDuration();
        }
        stickmanAnimation = AnimationLoaderFX.getInstance().loadAnimation(mStickmanStage.getStickmanFX(actor), name, 500, false); // TODO: with regard to get a "good" timing, consult the gesticon
        if (activity instanceof ActionMouthActivity) {
            stickmanAnimation.mParameter = ((ActionMouthActivity) activity).getWortTimeMark();
        }
        if (stickmanAnimation != null) {
            executeAnimation(stickmanAnimation);
        }
    }

    private void actionSetVoice(AbstractActivity activity, AgentConfig agent) {
        for (ActionFeature feat : activity.getFeatureList()) {
            if (feat.getKey().equalsIgnoreCase("voice")) {
                languageAgentMap.put(agent.getAgentName(), feat.getVal());
            }
        }
    }

    private void handleEmptyTextActivity(SpeechActivity sa) {
        //Mainly use for setting a new voice while playing script
        LinkedList<String> timemarks = sa.getTimeMarks("$");
        for (String tm : timemarks) {
            mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
        }
    }

    private String getLangVoiceFromConfig(String actor){
        AgentConfig agent = mProject.getAgentConfig(actor);
        String langVoince =  languageAgentMap.get(agent.getAgentName());
        if(langVoince == null || langVoince.equals("")){
            langVoince =  agent.getProperty("default-voice");
        }
        return langVoince;
    }

    protected void executeAnimation(AnimationFX stickmanAnimation) {
        // executeAnimation command to platform
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(stickmanAnimation, iosw);
        try {
            broadcast(new String(out.toByteArray(), "UTF-8").replace("\n", " "));
            out.close();
        } catch (UnsupportedEncodingException exc) {
            mLogger.warning(exc.getMessage());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void executeSpeachAndWait(  String executionId) {
        waitForSpeechToFinish(executionId);

    }



    private void waitForSpeechToFinish(String executionId) {
        synchronized (mActivityWorkerMap) {
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(executionId, cAW);
            Thread thread = getSpeakThread(executionId);
            thread.start();
            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }
            mLogger.warning("ActivityWorker " + executionId + " done ....");
        }
    }

    private Thread getSpeakThread(final String executionId) {
        return new Thread(){
            public void run(){
                System.out.println("ExecutionID: " + executionId);
                intentToSpeak(executionId);
            }
        };
    }

    public String intentToSpeak(String  executionId ){
        synchronized (mActivityWorkerMap) {
            String spokenText = "";
            SpeakerActivity speaker = speechActivities.get(executionId);
            try {
                spokenText = speaker.speak(executionId);
            } catch (Exception e) {
                e.printStackTrace();
            }
            return spokenText;
        }
    }

    @Override
    public void launch() {
        try {
            launchMaryTTSAndDialog();
        } catch (Exception e) {
            e.printStackTrace();
        }
        mListener = new StickmanTtsListener(8000, this);
        mListener.start();
        launchStickmanClient();
        addStickmansToStage();
        waitForClients();
    }

    private void launchStickmanClient() {
        for (ConfigFeature cf : mConfig.getEntryList()) {
            mLogger.message("Stickman Plugin Config: " + cf.getKey() + " = " + cf.getValue());
        }
        final boolean showStickmanNames = mConfig.containsKey("showstickmanname") ? mConfig.getProperty("showstickmanname").equalsIgnoreCase("true") : true;
        mDeviceName = mConfig.getPluginName();
        final String host = mConfig.getProperty("smhost");
        final String port = mConfig.getProperty("smport");
        // Start the StickmanStage client application
        mLogger.message("Starting StickmanStage Client Application ...");
        mLogger.message("Starting StickmanStage Client Application ...");
        getStickmanStage(showStickmanNames, host, port);
    }

    private void getStickmanStage(boolean showStickmanNames, String host, String port) {
        if (mConfig.containsKey("fullscreen")) {
            if (mConfig.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
                mStickmanStage = StickmanStageFX.getNetworkInstanceFullScreen(host, Integer.parseInt(port));
            } else {
                mStickmanStage = StickmanStageFX.getNetworkInstance(host, Integer.parseInt(port));
            }
        } else {
            mStickmanStage = StickmanStageFX.getNetworkInstance(host, Integer.parseInt(port));
        }
        mStickmanStage.showStickmanNameFX(showStickmanNames);
    }

    private void waitForClients() {
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for StickmanStage to launch");
            try {
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {
                mLogger.failure("Error while waiting ...");
            }
        }
    }

    private void launchMaryTTSAndDialog() throws Exception {
        WaitingDialog InfoDialog  = new WaitingDialog("Loading MaryTTS...");
        marySelfServer.registerObserver(InfoDialog);
        Thread tDialog = new Thread() {
            public void run(){
                try {
                    marySelfServer.startMaryServer(); //TODO: Show info dialog of loading....
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        tDialog.start();
        InfoDialog.setModal(true);
        InfoDialog.setVisible(true);
    }


    private void addStickmansToStage( ){
        for (AgentConfig agent:mProject.getProjectConfig().getAgentConfigList()) {
            if(agent.getDeviceName().equalsIgnoreCase(mDeviceName) ){
                mStickmanStage.addStickmanFX(agent.getAgentName());
            }
        }
        stickmanLaunchThread = new Thread() {
            public void run() {
                try {


                    StickmanStageFX.lauchStickman(mProject.getProjectPath());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };


        stickmanLaunchThread.start();
    }

    @Override
    public void unload() {
        // clear the stage
        try {
            stopClientsAndServers();
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        } finally {
            clearMaps();
            mStickmanStage.clearStage();
        }
    }

    private void stopClientsAndServers() throws InterruptedException, IOException {
        stopClients();
        marySelfServer.stopMaryServer();
        mListener.abort();
        mListener.join();
        mLogger.message("Joining server thread");
    }

    private void clearMaps() {
        mClientMap.clear();
        languageAgentMap.clear();
        wtsMap.clear();
        mActivityWorkerMap.clear();
    }

    private void stopClients() throws InterruptedException {
        for (final StickmanTtsHandler client : mClientMap.values()) {
            client.abort();
            client.join();
        }
    }

    public void scheduleSpeech(String id){
        SpeakerActivity speakerActivity = (SpeakerActivity) speechActivities.remove(id);
        SpeechActivity activity = speakerActivity.getSpeechActivity();
        final WordTimeMarkSequence wts = wtsMap.remove(id);
        final String actor = activity.getActor();
        final String langVoice = getLangVoiceFromConfig(actor);
        final AgentConfig agent = mProject.getAgentConfig(actor);
        final String voice = agent.getProperty(langVoice);
        final VoiceName voiceName = new VoiceName(voice);
        //MaryTTsSpeaker marySpeak = new MaryTTsSpeaker(activity, langVoice, voiceName);
        //speechToMouth(actor, marySpeak, wts);
        SpeakerTts ttsSpeak =  speakerActivity.getTtsSpeak();
        speechToMouth(actor, ttsSpeak, wts);
    }

    private void speechToMouth(String actor, SpeakerTts marySpeak, WordTimeMarkSequence wts) {
        System.out.println("Entered mouth");
        LinkedList blocks = marySpeak.getSpeechActivityTextBlocs();
        int wordIndex = 0;
        int totalTime = 0;
        for (final Object item : blocks) {
            if (!item.toString().contains("$")) {
                LinkedList<Phoneme> wordPhonemes = marySpeak.getWordPhonemeList(wordIndex);
                for (Phoneme p : wordPhonemes) {
                    if (p.getLipPosition() == null) {
                        continue;
                    }
                    mScheduler.schedule((int) p.getmStart(), null, new ActionMouthActivity(actor, "face", "Mouth_" + p.getLipPosition(), null, (int) (p.getmEnd() - p.getmStart()), wts), mProject.getAgentDevice(actor));
                    totalTime+= (int) (p.getmEnd() - p.getmStart());
                }
                wordIndex++;
            }
        }
        //Clossing the mouth
        mScheduler.schedule(totalTime + 100, null, new ActionMouthActivity(actor, "face", "Mouth_Default", null, 300, wts), mProject.getAgentDevice(actor));
        AnimationFX stickmanAnimation = new AnimationFX();
        stickmanAnimation = AnimationLoaderFX.getInstance().loadEventAnimation(mStickmanStage.getStickmanFX(actor), "Speaking", 3000, false);
        stickmanAnimation.mParameter = wts;
        executeAnimation(stickmanAnimation);
    }

    // Handle some message
    public void handle(final String message, final StickmanTtsHandler client) {
        mLogger.message("Handling " + message + "");
        if (message.contains("#ANIM#end#")) {
            handleAnimation(message);
        } else if (message.contains("$")) {
            handleAction(message);
        } else if (message.contains("#AUDIO#end#")) {
            handleAudio(message);
        }


    }

    private void handleAudio(String message) {
        synchronized (mActivityWorkerMap) {
            int start = message.lastIndexOf("#") + 1;
            String event_id = message.substring(start);
            mActivityWorkerMap.remove(event_id);
            mActivityWorkerMap.notifyAll();
        }
    }

    private void handleAction(String message) {
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.notifyAll();
            mProject.getRunTimePlayer().getActivityScheduler().handle(message);
        }
    }

    private void handleAnimation(String message) {
        synchronized (mActivityWorkerMap) {
            int start = message.lastIndexOf("#") + 1;
            String animId = message.substring(start);
            if (mActivityWorkerMap.containsKey(animId)) {
                mActivityWorkerMap.remove(animId);
            }
            mActivityWorkerMap.notifyAll();
        }
    }

    // Broadcast some message
    private void broadcast(final String message) {
        for (final StickmanTtsHandler client : mClientMap.values()) {
            client.send(message);
        }
    }

}
