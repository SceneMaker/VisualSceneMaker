package de.dfki.vsm.xtension.voiceover;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.tts.SpeakerTts;
import de.dfki.vsm.util.tts.TTSFactory;
import de.dfki.vsm.util.tts.marytts.MaryTTsProcess;
import de.dfki.vsm.xtension.stickmantts.util.tts.SpeakerActivity;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.LineStop;

import java.io.IOException;
import java.util.HashMap;

/**
 * Created by alvaro on 6/4/16.
 */
public class VoiceOverExecutor extends ActivityExecutor implements EventListener {
    private MaryTTsProcess marySelfServer;
    private int maryExecutionId=0;
    private final HashMap<String, ActivityWorker> mActivityWorkerMap = new HashMap();
    private final EventDispatcher mEventDispatcher = EventDispatcher.getInstance();

    public VoiceOverExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        marySelfServer = MaryTTsProcess.getsInstance(mConfig.getProperty("mary.base"));
        mEventDispatcher.register(this);
    }

    @Override
    public String marker(long id) {
        return "$__" + id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        if(activity instanceof SpeechActivity){
            actionExecuteSpeechAndWait(activity);
        }
    }

    private void actionExecuteSpeechAndWait(AbstractActivity activity) {
        String executionId = getExecutionId();
        waitForSpeachToFinish(executionId, activity);

    }


    public  String getExecutionId() {
        return "mary_" + maryExecutionId++;
    }

    private void waitForSpeachToFinish(String  executionId, AbstractActivity activity){
        synchronized (mActivityWorkerMap){
            ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(executionId, cAW);
            Thread thread = getSpeakThread(activity, executionId);
            thread.start();
            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }
        }
    }


    private Thread getSpeakThread(final AbstractActivity activity, final String executionId) {
        return new Thread(){
            public void run(){
                intentToSpeak(executionId, activity);
            }
        };
    }

    public String intentToSpeak(String  executionId, AbstractActivity activity){
        synchronized (mActivityWorkerMap) {
            TTSFactory factoryTTs = new TTSFactory(mConfig, getSpeechActivityFromAbstract(activity), mProject);
            SpeakerTts speakerTts = factoryTTs.getTTs();
            SpeakerActivity speakerActivity = new SpeakerActivity(speakerTts);
            String spokenText = "";
            try {
                spokenText = speakerActivity.speak(executionId);
            } catch (Exception e) {
                e.printStackTrace();
            }
            return spokenText;
        }
    }

    private SpeechActivity getSpeechActivityFromAbstract(AbstractActivity activity){
        return (SpeechActivity) activity;
    }

    @Override
    public void launch() {
        try {
            launchMary();
        } catch (Exception e) {
            unload();
            e.printStackTrace();
        }
    }

    private void launchMary() throws Exception {
        marySelfServer.startMaryServer(); //TODO: Show info dialog of loading....
    }


    @Override
    public void unload() {
        try {
            stopMaryServer();
        } catch (IOException e) {
            e.printStackTrace();
        }finally {
            mActivityWorkerMap.clear();
        }
    }

    private void stopMaryServer() throws IOException {
        marySelfServer.stopMaryServer();
    }

    public int getHowManyWorkers(){
        return mActivityWorkerMap.size();
    }

    @Override
    public void update(EventObject event) {
        if(event instanceof LineStop){
            String executionId = ((LineStop) event).getExecutionId();
            handleAudioEnd(executionId);
        }
    }

    private void handleAudioEnd(String executionId){
        synchronized (mActivityWorkerMap){
            mActivityWorkerMap.remove(executionId);
            mActivityWorkerMap.notifyAll();
        }
    }
}
