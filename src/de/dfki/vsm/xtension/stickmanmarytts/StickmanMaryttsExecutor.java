/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.stickmanmarytts;

import de.dfki.action.sequence.WordTimeMarkSequence;
import de.dfki.stickman.StickmanStage;
import de.dfki.stickman.animationlogic.Animation;
import de.dfki.stickman.animationlogic.AnimationLoader;
import de.dfki.util.xml.XMLUtilities;
import de.dfki.util.ios.IOSIndentWriter;
import de.dfki.vsm.editor.dialog.WaitingDialog;
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
import de.dfki.vsm.util.tts.MaryTTsProcess;
import de.dfki.vsm.util.tts.MaryTTsSpeaker;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.VoiceName;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.sequence.Phoneme;
import de.dfki.vsm.xtension.stickmanmarytts.action.ActionMouthActivity;

import java.io.*;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 */
public class StickmanMaryttsExecutor extends ActivityExecutor {

    private static StickmanStage mStickmanStage;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private StickmanMaryttsListener mListener;
    private final HashMap<String, StickmanMaryttsHandler> mClientMap = new HashMap();
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    private HashMap<String, String> languageAgentMap;
    private HashMap<String, AbstractActivity> speechActivities = new HashMap<>();
    private HashMap<String, WordTimeMarkSequence> wtsMap = new HashMap<>();
    private MaryTTsProcess marySelfServer;
    private int maryId;

    public StickmanMaryttsExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
        maryId = 0;
        languageAgentMap = new HashMap<>();
        marySelfServer = new MaryTTsProcess(mConfig.getProperty("mary.base"));
    }

    public void accept(final Socket socket) {
        final StickmanMaryttsHandler client = new StickmanMaryttsHandler(socket, this);
        // TODO: Get some reasonable name for references here!
        mClientMap.put(client.getName(), client);
        client.start();
        mLogger.warning("Accepting " + client.getName() + "");
    }

    @Override
    public final String marker(final long id) {
        // Stickman style bookmarks
        return "$" + id;
    }

    private final String getExecutionId() {
        return "mary_" + maryId++;
    }

    @Override
    public void execute(AbstractActivity activity) {
        final String name = activity.getName();
        if (activity instanceof SpeechActivity) {
            actionExecuteSpeech(activity);
        } else if (activity instanceof ActionActivity || activity instanceof ActionMouthActivity) {
            if (name.equalsIgnoreCase("set") && activity instanceof ActionActivity) {
                actionSetVoice(activity);
            } else {
                actionLoadAnimation(activity);
            }
        }
    }

    private void actionExecuteSpeech(AbstractActivity activity) {
        final String actor = activity.getActor();
        AgentConfig agent = mProject.getAgentConfig(actor);
        String langVoice = getLangVoiceFromConfig(actor);
        String voice = agent.getProperty(langVoice);
        VoiceName voiceName = new VoiceName(voice);
        SpeechActivity sa = (SpeechActivity) activity;
        String activityText = sa.getTextOnly("$").trim();
        if (activityText.isEmpty()) {
            handleEmptyTextActivity(sa);
        }
        MaryTTsSpeaker marySpeak = new MaryTTsSpeaker(sa, langVoice, voiceName);
        String executionId = getExecutionId();
        WordTimeMarkSequence wts = marySpeak.getWordTimeSequence();
        //We will use these two later
        speechActivities.put(executionId, activity);
        wtsMap.put(executionId, wts);
        executeSpeachAndWait(marySpeak, executionId);
    }

    private void actionSetVoice(AbstractActivity activity) {
        AgentConfig agent = mProject.getAgentConfig(activity.getActor());
        for (ActionFeature feat : activity.getFeatureList()) {
            if (feat.getKey().equalsIgnoreCase("voice")) {
                languageAgentMap.put(agent.getAgentName(), feat.getVal());
            }
        }
    }

    private void actionLoadAnimation(AbstractActivity activity) {
        Animation stickmanAnimation;
        int duration = 500;
        if (activity instanceof ActionMouthActivity) {
            duration = ((ActionMouthActivity) activity).getDuration();
        }
        stickmanAnimation = AnimationLoader.getInstance().loadAnimation(mStickmanStage.getStickman(activity.getActor()),
                activity.getName(), duration, false);
        if (activity instanceof ActionMouthActivity) {
            
            
            
            stickmanAnimation.mParameter = ((ActionMouthActivity) activity).getWortTimeMark();
        }
        if (stickmanAnimation != null) {
            executeAnimation(stickmanAnimation);
        }
    }

    private void handleEmptyTextActivity(SpeechActivity sa) {
        //Mainly use for setting a new voice while playing script
        LinkedList<String> timemarks = sa.getTimeMarks("$");
        for (String tm : timemarks) {
            mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
        }
    }

    private String getLangVoiceFromConfig(String actor) {
        AgentConfig agent = mProject.getAgentConfig(actor);
        String langVoince = languageAgentMap.get(agent.getAgentName());
        if (langVoince == null || langVoince.equals("")) {
            langVoince = agent.getProperty("default-voice");
        }
        return langVoince;
    }

    protected void executeAnimation(Animation stickmanAnimation) {
        // executeAnimation command to platform
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOSIndentWriter iosw = new IOSIndentWriter(out);
        boolean r = XMLUtilities.writeToXMLWriter(stickmanAnimation, iosw);
        
        String message = "";
        // Fuck German Umlaute and Encoding
        message = out.toString().replace("ö", "oe").replace("ä", "ae").replace("ü", "ue").replace("Ö", "Oe").replace("Ä", "Ae").replace("Ü", "Ue").replace("ß", "ss").replace("\n", " ").replace("   ", " ").replace("  ", " ");
        try {
            //broadcast(new String(out.toByteArray(), "UTF-8").replace("\n", " "));
            broadcast(message);
            out.close();
        } catch (UnsupportedEncodingException exc) {
            mLogger.warning(exc.getMessage());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void executeSpeachAndWait(MaryTTsSpeaker marySpeak, String executionId) {
        synchronized (mActivityWorkerMap) {
            String spokenText = "";
            try {
                spokenText = marySpeak.speak(executionId);
            } catch (Exception e) {
                e.printStackTrace();
            }
            if (spokenText.length() > 0) {
                ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                mActivityWorkerMap.put(executionId, cAW);
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
    }

    @Override
    public void launch() {
        try {
            launchMaryTTSAndDialog();
        } catch (Exception e) {
            e.printStackTrace();
        }
        mListener = new StickmanMaryttsListener(8000, this);
        mListener.start();
        launchStickmanClient();
        waitForStickman();
    }

    private void waitForStickman() {
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for StickmanStage to launch");
            try {
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {
                mLogger.failure("Error while waiting ...");
            }
        }
    }

    private void launchStickmanClient() {
        mLogger.message("Starting StickmanStage Client Application ...");
        final String host = mConfig.getProperty("smhost");
        final String port = mConfig.getProperty("smport");
        if (mConfig.containsKey("fullscreen")) {
            if (mConfig.getProperty("fullscreen").equalsIgnoreCase(Boolean.TRUE.toString())) {
                mStickmanStage = StickmanStage.getNetworkInstanceFullScreen(host, Integer.parseInt(port));
            } else {
                mStickmanStage = StickmanStage.getNetworkInstance(host, Integer.parseInt(port));
            }
        } else {
            mStickmanStage = StickmanStage.getNetworkInstance(host, Integer.parseInt(port));
        }
        

        addStickmansToStage();
    }

    private void launchMaryTTSAndDialog() throws Exception {
        WaitingDialog InfoDialog = new WaitingDialog("Loading MaryTTS...");
        marySelfServer.registerObserver(InfoDialog);
        Thread tDialog = getThreadMaryServer();
        tDialog.start();
        InfoDialog.setModal(true);
        InfoDialog.setVisible(true);
    }

    private Thread getThreadMaryServer() {
        Thread tDialog = new Thread() {
            public void run() {
                try {
                    marySelfServer.startMaryServer();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        return tDialog;
    }

    private void addStickmansToStage() {
        for (AgentConfig agent : mProject.getProjectConfig().getAgentConfigList()) {
            if (agent.getDeviceName().equalsIgnoreCase("stickmanmarytts") || agent.getDeviceName().equalsIgnoreCase("stickman")) {
                StickmanStage.addStickman(agent.getAgentName());
            }
        }
    }

    @Override
    public void unload() {
        StickmanStage.clearStage();
        unloadClients();
        languageAgentMap.clear();
        wtsMap.clear();
        try {
            marySelfServer.stopMaryServer();
            mListener.abort();
            mListener.join();
            mLogger.message("Joining server thread");
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }
    }

    private void unloadClients() {
        for (final StickmanMaryttsHandler client : mClientMap.values()) {
            client.abort();
            try {
                client.join();
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
                mLogger.message("Joining client thread");
            }
        }
        mClientMap.clear();
    }

    public void scheduleSpeech(String id) {
        SpeechActivity activity = (SpeechActivity) speechActivities.remove(id);
        final WordTimeMarkSequence wts = wtsMap.remove(id);
        final String actor = activity.getActor();
        final String langVoice = getLangVoiceFromConfig(actor);
        final AgentConfig agent = mProject.getAgentConfig(actor);
        final String voice = agent.getProperty(langVoice);
        final VoiceName voiceName = new VoiceName(voice);
        MaryTTsSpeaker marySpeak = new MaryTTsSpeaker(activity, langVoice, voiceName);
        speechToMouth(actor, marySpeak, wts);
    }

    private void speechToMouth(String actor, MaryTTsSpeaker marySpeak, WordTimeMarkSequence wts) {
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
                    totalTime += (int) (p.getmEnd() - p.getmStart());
                }
                wordIndex++;
            }
        }
        //Clossing the mouth
        mScheduler.schedule(totalTime + 100, null, new ActionMouthActivity(actor, "face", "Mouth_Default", null, 300, wts), mProject.getAgentDevice(actor));
        Animation stickmanAnimation = new Animation();
        stickmanAnimation = AnimationLoader.getInstance().loadEventAnimation(mStickmanStage.getStickman(actor), "Speaking", 3000, false);
        stickmanAnimation.mParameter = wts;
        executeAnimation(stickmanAnimation);
    }

    // Handle some message
    public void handle(final String message, final StickmanMaryttsHandler client) {
        mLogger.message("Handling " + message + "");
        if (message.contains("#ANIM#end#")) {
            handleAnimationResponse(message);
        } else if (message.contains("$")) {
            handleGenericResponse(message);
        } else if (message.contains("#AUDIO#end#")) {
            handleMaryTTSResponse(message);
        }

    }

    private void handleAnimationResponse(String message) {
        synchronized (mActivityWorkerMap) {
            int start = message.lastIndexOf("#") + 1;
            String animId = message.substring(start);
            if (mActivityWorkerMap.containsKey(animId)) {
                mActivityWorkerMap.remove(animId);
            }
            // wake me up ..
            mActivityWorkerMap.notifyAll();
        }
    }

    private void handleMaryTTSResponse(String message) {
        synchronized (mActivityWorkerMap) {
            int start = message.lastIndexOf("#") + 1;
            String event_id = message.substring(start);
            mActivityWorkerMap.remove(event_id);
            mActivityWorkerMap.notifyAll();
        }
    }

    private void handleGenericResponse(String message) {
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.notifyAll();
            mProject.getRunTimePlayer().getActivityScheduler().handle(message);
        }
    }

    // Broadcast some message
    private void broadcast(final String message) {
        for (final StickmanMaryttsHandler client : mClientMap.values()) {
            client.send(message);
        }
    }

}
