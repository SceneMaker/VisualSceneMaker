package de.dfki.vsm.xtension.charamelWs;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.WordMapping;

import java.util.LinkedList;
import java.util.List;

public abstract class CharacterPlugin extends ActivityExecutor {
    protected WordMapping mWordMapping = new WordMapping();
    String markerSign;

    public CharacterPlugin(PluginConfig config, RunTimeProject project, String markerSign) {
        super(config, project);
        this.markerSign = markerSign;
    }

    @Override
    public abstract String marker(long id);

    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            final SpeechActivity speech_activity = (SpeechActivity) activity;
            final String speech_text = speech_activity.getTextOnly(markerSign).trim();
            final List<String> time_marks = speech_activity.getTimeMarks(markerSign);

            if (speech_text.isEmpty()) {
                // If speech_text is empty we assume that the activity has
                // empty speech text but has marker activities registered
                for (final String tm : time_marks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                String activity_actor = activity.getActor();
                // load wordmapping database
                mWordMapping.load(activity_actor, mProject);
                // do the pronounciation mapping
                speech_activity.doPronounciationMapping(mWordMapping);
                // get the charamel avatar id
                String aid = mProject.getAgentConfig(activity_actor).getProperty("aid");
                // build action
                speak(speech_activity.getBlocks(), speech_activity.getPunct(), aid);
            }
        } else {
            act((ActionActivity) activity);
        }
    }

    protected abstract void speak(LinkedList blocks, String punct, String aid);

    abstract void speak(SpeechActivity speech);

    abstract void act(ActionActivity action);
}
