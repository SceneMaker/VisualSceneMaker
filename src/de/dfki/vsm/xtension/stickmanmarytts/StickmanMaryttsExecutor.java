/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.stickmanmarytts;

import de.dfki.action.sequence.TimeMark;
import de.dfki.action.sequence.Word;
import de.dfki.action.sequence.WordTimeMarkSequence;
import de.dfki.stickman.Stickman;
import de.dfki.stickman.StickmanStage;
import de.dfki.stickman.animationlogic.Animation;
import de.dfki.stickman.animationlogic.AnimationLoader;
import de.dfki.util.xml.XMLUtilities;
import de.dfki.util.ios.IOSIndentWriter;
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
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.I4GMaryClient;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.MaryStickmanPhonemes;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.VoiceName;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.sequence.Phoneme;
import de.dfki.vsm.xtension.stickmanmarytts.action.ActionMouthActivity;

import java.io.*;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 *
 * @author Patrick Gebhard
 */
public class StickmanMaryttsExecutor extends ActivityExecutor {

    // The stickman stage window
    private static StickmanStage mStickmanStage;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The tworld listener
    private StickmanMaryttsListener mListener;
    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();
    // The client thread list
    private final HashMap<String, StickmanMaryttsHandler> mClientMap = new HashMap();
    // The map of activity worker
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    private final String OS = System.getProperty("os.name").toLowerCase();
    private HashMap<String, String> languageAgentMap;
    private HashMap<String, AbstractActivity> speechActivities = new HashMap<>();
    private HashMap<String, WordTimeMarkSequence> wtsMap= new HashMap<>();

    private I4GMaryClient maryTTs;
    private int maryId;

    // Construct the executor
    public StickmanMaryttsExecutor(final PluginConfig config, final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
        //maryTTs = I4GMaryClient.instance();
        maryId = 0;
        languageAgentMap = new HashMap<>();

    }

    // Accept some socket
    public void accept(final Socket socket) {
        // Make new client thread 
        final StickmanMaryttsHandler client = new StickmanMaryttsHandler(socket, this);
        // Add the client to list
        // TODO: Get some reasonable name for references here!
        mClientMap.put(client.getName(), client);
        // Start the client thread
        client.start();
        //
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
        // get action information
        final String actor = activity.getActor();
        final String name = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatureList();


        if (maryTTs == null) {
            try {
                maryTTs = I4GMaryClient.instance();
            } catch (Exception e) {
                System.out.println("MaryTT not initiated yet");
            }

        }
        Animation stickmanAnimation = new Animation();
        AgentConfig agent = mProject.getAgentConfig(actor);
        String langVoince =  languageAgentMap.get(agent.getAgentName());
        if(langVoince == null || langVoince.equals("")){
            langVoince =  agent.getProperty("default-voice");
        }
        String voice = agent.getProperty(langVoince);
        int index = 0;
        VoiceName voiceName = new VoiceName(voice);
        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            WordTimeMarkSequence wts = new WordTimeMarkSequence(sa.getTextOnly("$"));
            LinkedList<String> timemarks = sa.getTimeMarks("$");
            String text = sa.getTextOnly("$").trim();
            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }

            LinkedList blocks = sa.getBlocks();
            for (final Object item : blocks) {
                if (!item.toString().contains("$")) {
                    Word w = new Word(item.toString());
                    wts.add(w);
                    maryTTs.addWord(item.toString());
                    index++;
                } else {
                    wts.add(new TimeMark(item.toString()));
                }
            }

            String executionId = getExecutionId();
            speechActivities.put(executionId, activity);
            wtsMap.put(executionId, wts);
            executeSpeachAndWait(activity, mStickmanStage.getStickman(actor).mType, executionId, voiceName, langVoince);

        } else if (activity instanceof ActionActivity || activity instanceof ActionMouthActivity) {

            if (name.equalsIgnoreCase("set") && activity instanceof ActionActivity) {
                for (ActionFeature feat : activity.getFeatureList()) {
                    if (feat.getKey().equalsIgnoreCase("voice")) {
                        languageAgentMap.put(agent.getAgentName(), feat.getVal());
                    }
                }
            } else {
                int duration = 500;
                if (activity instanceof ActionMouthActivity) {
                    duration = ((ActionMouthActivity) activity).getDuration();
                }
                stickmanAnimation = AnimationLoader.getInstance().loadAnimation(mStickmanStage.getStickman(actor), name, duration, false); // TODO: with regard to get a "good" timing, consult the gesticon
                if (activity instanceof ActionMouthActivity) {
                    stickmanAnimation.mParameter = ((ActionMouthActivity) activity).getWortTimeMark();
                }
                if (stickmanAnimation != null) {
                    executeAnimation(stickmanAnimation);
                }
            }
        }
    }

    public void scheduleSpeech(String id){
        System.out.println("Scheduled speech");
        SpeechActivity activity = (SpeechActivity) speechActivities.remove(id);
        WordTimeMarkSequence wts = wtsMap.remove(id);
        final String actor = activity.getActor();
        AgentConfig agent = mProject.getAgentConfig(actor);
        String langVoince =  languageAgentMap.get(agent.getAgentName());
        if(langVoince == null || langVoince.equals("")){
            langVoince =  agent.getProperty("default-voice");
        }
        String voice = agent.getProperty(langVoince);
        VoiceName voiceName = new VoiceName(voice);
        MouthSpeech(actor, langVoince, voiceName, activity, wts);
        System.out.println("End mouth");
    }

    private void MouthSpeech(String actor, String langVoince, VoiceName voiceName, SpeechActivity sa, WordTimeMarkSequence wts) {
       // WordTimeMarkSequence wts = new WordTimeMarkSequence(sa.getTextOnly("$"));
        System.out.println("Entered mouth");
        LinkedList blocks = sa.getBlocks();
        HashMap<Integer, LinkedList<Phoneme>> phonemes = new HashMap<>();
        int index = 0;
        MaryStickmanPhonemes mary = new MaryStickmanPhonemes();
        phonemes = mary.getPhonemesAndMouthPosition(sa, mStickmanStage.getStickman(actor).mType, voiceName, langVoince);
        int lastPhoneme = 0;
        if (phonemes.size() > 0) {
            lastPhoneme = (int) phonemes.get(phonemes.size() - 1).getLast().getmEnd();
        }
        for (final Object item : blocks) {
            if (!item.toString().contains("$")) {
                Word w = new Word(item.toString());
                //wts.add(w);
                //maryTTs.addWord(item.toString());
                LinkedList<Phoneme> wordPhonemes = phonemes.get(index);
                for (Phoneme p : wordPhonemes) {
                    if (p.getLipPosition() == null) {
                        continue;
                    }
                    mScheduler.schedule((int) p.getmStart(), null, new ActionMouthActivity(actor, "face", "Mouth_" + p.getLipPosition(), null, (int) (p.getmEnd() - p.getmStart()), wts), mProject.getAgentDevice(actor));
                }

                index++;
            }
        }
        //Clossing the mouth
        System.out.println("Schedula enumagion");
        mScheduler.schedule(lastPhoneme + 100, null, new ActionMouthActivity(actor, "face", "Mouth_Default", null, 300, wts), mProject.getAgentDevice(actor));
        Animation stickmanAnimation = new Animation();
        stickmanAnimation = AnimationLoader.getInstance().loadEventAnimation(mStickmanStage.getStickman(actor), "Speaking", 3000, false);
        stickmanAnimation.mParameter = wts;
        executeAnimation(stickmanAnimation);
    }

    protected void executeAnimation(Animation stickmanAnimation) {
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

    protected void executeSpeachAndWait(AbstractActivity activity, Stickman.TYPE gender, String executionId, VoiceName voiceName, String language) {
        String text = ((SpeechActivity) activity).getTextOnly("$");
        synchronized (mActivityWorkerMap) {
            String phrase = maryTTs.getText();
            if (phrase.length() > 0) {

                try {
                    maryTTs.speak(gender, executionId, voiceName, language);
                } catch (Exception e) {
                    e.printStackTrace();
                }

                // organize wait for feedback
                ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                mActivityWorkerMap.put(executionId, cAW);

                // wait until we got feedback
                mLogger.warning("ActivityWorker " + executionId + " waiting ....");

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

    private void executeAnimationAndWait(AbstractActivity activity, Animation stickmanAnimation, String animId) {
        // executeAnimation command to platform 
        synchronized (mActivityWorkerMap) {
            // executeAnimation command to platform 
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            IOSIndentWriter iosw = new IOSIndentWriter(out);
            boolean r = XMLUtilities.writeToXMLWriter(stickmanAnimation, iosw);

            try {
                broadcast(new String(out.toByteArray(), "UTF-8").replace("\n", " "));
            } catch (UnsupportedEncodingException exc) {
                mLogger.warning(exc.getMessage());
            }

            // organize wait for feedback
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(animId, cAW);

            // wait until we got feedback
            mLogger.message("ActivityWorker " + animId + " waiting ....");

            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }

            mLogger.message("ActivityWorker " + animId + "  done ....");
        }
        // Return when terminated
    }

    @Override
    public void launch() {
        // Create the connection
        mListener = new StickmanMaryttsListener(8000, this);
        // Start the connection
        mListener.start();

        //Starting MaryTTS Server
        final String maryttsBaseDir = mConfig.getProperty("mary.base");

        // Create the plugin's processes
        String cmd = "";
        String cmdName = "";
        ProcessBuilder pb = null;
        cmd = maryttsBaseDir + File.separator + "bin" + File.separator + "marytts-server";
        //Check for existence
        File f = new File(cmd);
        boolean exists  = false;
        String message = "";
        if(f.exists() && !f.isDirectory()) {
            // do something
            exists = true;
            message = "Loading MaryTTS ...";
        }else{
            message = "The MaryTTS server could not be found. Please edit the project.xml";
        }
        if (isUnix() || isMac()) {
            cmdName = "marytts.server.Mary";
            pb = new ProcessBuilder("/bin/bash", cmd);
        } else if (isWindows()) {
            cmdName = "marytts.server.Mary";
            cmd = cmd + ".bat";
            String[] command = {"CMD", "/C", cmd};
            pb = new ProcessBuilder(command);
        }

        final JDialog info = new JDialog();
        info.setTitle("Info");
        JPanel messagePane = new JPanel();
        messagePane.add(new JLabel(message));
        info.add(messagePane);
        info.pack();
        info.setLocationRelativeTo(null);

        final ProcessBuilder processB = pb;
        final String command = cmdName;
        final boolean MaryTTSExists = exists;
        Thread tDialog = new Thread() {
            public void run() {
                if(MaryTTSExists) {
                    try {
                        processB.redirectErrorStream(true);
                        Process p = processB.start();
                        mProcessMap.put(command, p);
                        InputStream is = p.getInputStream();
                        InputStreamReader isr = new InputStreamReader(is);
                        BufferedReader br = new BufferedReader(isr);
                        String line;
                        boolean started = false;
                        while (!started) {
                            line = br.readLine();
                            if (line.contains("started in")) {
                                started = true;
                                info.setVisible(false);
                                info.dispose();
                                is.close();
                                isr.close();
                            }
                            System.out.println(line);
                        }
                    } catch (final Exception exc) {
                        mLogger.failure(exc.toString());
                    }
                }else{
                    try {
                        Thread.sleep(2000);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    info.dispose();
                }
            }
        };



        tDialog.start();
        info.setModal(true);
        info.setVisible(true);


        // Get the plugin configuration
        for (ConfigFeature cf : mConfig.getEntryList()) {
            mLogger.message("Stickman Plugin Config: " + cf.getKey() + " = " + cf.getValue());
        }

        final String host = mConfig.getProperty("smhost");
        final String port = mConfig.getProperty("smport");

        // Start the StickmanStage client application
        mLogger.message("Starting StickmanStage Client Application ...");
        mStickmanStage = StickmanStage.getNetworkInstance(host, Integer.parseInt(port));
        addStickmansToStage();


        // wait for stickman stage
        while (mClientMap.isEmpty()) {
            mLogger.message("Waiting for StickmanStage");
            try {
                Thread.sleep(1000);
            } catch (final InterruptedException exc) {
                mLogger.failure("Error while waiting ...");
            }
        }
    }

    private void addStickmansToStage( ){
        for (AgentConfig agent:mProject.getProjectConfig().getAgentConfigList()) {
            if(agent.getDeviceName().equalsIgnoreCase("stickmanmarytts") || agent.getDeviceName().equalsIgnoreCase("stickman")){
                // TODO - read config
                StickmanStage.addStickman(agent.getAgentName());
            }
        }
    }

    @Override
    public void unload() {
        // clear the stage
        StickmanStage.clearStage();
        // Abort the client threads
        for (final StickmanMaryttsHandler client : mClientMap.values()) {
            client.abort();
            // Join the client thread
            try {
                client.join();
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
                // Print some information 
                mLogger.message("Joining client thread");
            }
        }
        // Clear the map of clients 
        mClientMap.clear();
        languageAgentMap.clear();
        wtsMap.clear();
        mActivityWorkerMap.clear();
        // Abort the server thread
        try {
            mListener.abort();
            // Join the client thread
            mListener.join();
            // Print some information 
            mLogger.message("Joining server thread");
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }

        // Wait for pawned processes
        for (final Map.Entry<String, Process> entry : mProcessMap.entrySet()) {
            // Get the process entry
            final String name = entry.getKey();
            final Process process = entry.getValue();
            String killCmd = "";

            try {
                process.destroyForcibly();
                // Kill the processes
                Process killer = null;
                if (isUnix() || isMac()) {
                    killCmd = "ps aux | grep '" + name + "' | awk '{print $2}' | xargs kill";
                    String[] cmd = {
                        "/bin/sh",
                        "-c",
                        killCmd
                    };
                    killer = Runtime.getRuntime().exec(cmd);
                } else if (isWindows()) {
                    killCmd = "wmic Path win32_process Where \"CommandLine Like '%" + name + "%'\" Call Terminate";
                    killer = Runtime.getRuntime().exec(killCmd);
                }

                killer.waitFor();
                // Print some information
                mLogger.message("Joining killer " + name + "");
                // Wait for the process
                process.waitFor();
                // Print some information
                mLogger.message("Joining process " + name + "");
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
            }
        }

        // Clear the map of processes
        mProcessMap.clear();
    }

    // Handle some message
    public void handle(final String message, final StickmanMaryttsHandler client) {
        mLogger.message("Handling " + message + "");

        synchronized (mActivityWorkerMap) {

            if (message.contains("#ANIM#end#")) {
                int start = message.lastIndexOf("#") + 1;
                String animId = message.substring(start);

                if (mActivityWorkerMap.containsKey(animId)) {
                    mActivityWorkerMap.remove(animId);
                }
                // wake me up ..
                mActivityWorkerMap.notifyAll();
            } else if (message.contains("$")) {
                // wake me up ..
                mActivityWorkerMap.notifyAll();
                // identify the related activity
                //AbstractActivity activity = mProject.getRunTimePlayer().getActivityScheduler().getMarkerActivity(message);
                // play the activity
                //mProject.getRunTimePlayer().getActivityScheduler().handle(new MarkerFeedback(activity, message));
                mProject.getRunTimePlayer().getActivityScheduler().handle(message);
            } else if (message.contains("#AUDIO#end#")) {
                int start = message.lastIndexOf("#") + 1;
                String event_id = message.substring(start);
                mActivityWorkerMap.remove(event_id);
                mActivityWorkerMap.notifyAll();

            }

        }
    }

// Broadcast some message
    private void broadcast(final String message) {
        for (final StickmanMaryttsHandler client : mClientMap.values()) {
            client.send(message);
        }
    }

    private boolean isWindows() {
        return (OS.indexOf("win") >= 0);
    }

    private boolean isMac() {
        return (OS.indexOf("mac") >= 0);
    }

    private boolean isUnix() {
        return (OS.indexOf("nix") >= 0 || OS.indexOf("nux") >= 0 || OS.indexOf("aix") > 0);
    }

}
