/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
//import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import javax.swing.SwingUtilities;

/**
 *
 * @author Patrick Gebhard
 */
public class QuestionnaireExecutor extends ActivityExecutor implements QuestionnaireListener {

    // The GUI
    QuestionnaireGUI mQuestionnaireGUI;
    // The current ActivityWorker
    ActivityWorker mActivityWorker = null;
    private final HashSet<ActivityWorker> mActivityWorkers = new HashSet<>();
    // Configuration values
    private final HashMap<String, String> mPersonalValues = new HashMap<>();
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public QuestionnaireExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
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

            if (name.equalsIgnoreCase("show")) {
                mQuestionnaireGUI.setVisible(true);

                // wait until we got feedback
                mLogger.message("ActivityWorker for Questionnaire waiting ....");
                mActivityWorker = (ActivityWorker) Thread.currentThread();
                mActivityWorkers.add(mActivityWorker);

                synchronized (mActivityWorker) {
                    while (mActivityWorkers.contains(mActivityWorker)) {
                        try {
                            mActivityWorker.wait();
                        } catch (InterruptedException exc) {
                            mLogger.failure(exc.toString());
                        }
                    }
                }

                mLogger.message("ActivityWorker for Questionnaire done ....");
            }

            if (name.equalsIgnoreCase("name")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("value")) {
                        mQuestionnaireGUI.setName(af.getVal());
                    }
                }
            }

            if (name.equalsIgnoreCase("age")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("value")) {
                        mQuestionnaireGUI.setAge(af.getVal());
                    }
                }
            }

            if (name.equalsIgnoreCase("sex")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("value")) {
                        mQuestionnaireGUI.setSex(af.getVal());
                    }
                }
            }

            if (name.equalsIgnoreCase("interviews")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("value")) {
                        mQuestionnaireGUI.setJobinterviews(af.getVal());
                    }
                }
            }

            if (name.equalsIgnoreCase("strength")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("value")) {
                        mQuestionnaireGUI.setStrength(af.getVal());
                    }
                }
            }

            if (name.equalsIgnoreCase("weakness")) {
                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().equalsIgnoreCase("value")) {
                        mQuestionnaireGUI.setWeakness(af.getVal());
                    }
                }
            }

            if (name.equalsIgnoreCase("next")) {
               mQuestionnaireGUI.next();
            }
        }
    }

    @Override
    public void launch() {
        mQuestionnaireGUI = new QuestionnaireGUI();

        for (ConfigFeature cf : mConfig.getEntryList()) {
            mPersonalValues.put(cf.getKey(), cf.getValue());
        }

        SwingUtilities.invokeLater(() -> mQuestionnaireGUI.init(this, mPersonalValues));
    }

    @Override
    public void unload() {
        // nothing
    }

    @Override
    public void updateOnUestionnaire(HashMap<String, String> uservalues) {
        // Handle updates from the questionnaire

        HashMap<String, AbstractValue> values = new HashMap<>();
        values.put("name", new StringValue(uservalues.get("name")));
        values.put("age", new IntValue(new Integer(uservalues.get("age"))));
        values.put("sex", new StringValue(uservalues.get("sex")));

        // do the interview thing
        int interviews = 0;
        String interviewsStr = uservalues.get("interviews");
        if (!interviewsStr.equalsIgnoreCase("keine")) {
            if (!interviewsStr.equalsIgnoreCase("mehr als 8")) {
                interviews = new Integer(interviewsStr);
            } else {
                interviews = 9;
            }
        }

        values.put("interviews", new IntValue(interviews));

        values.put("strength1", new BooleanValue(uservalues.get("strength1").equalsIgnoreCase("ja")));
        values.put("strength2", new BooleanValue(uservalues.get("strength2").equalsIgnoreCase("ja")));
        values.put("strength3", new BooleanValue(uservalues.get("strength3").equalsIgnoreCase("ja")));
        values.put("strength4", new BooleanValue(uservalues.get("strength4").equalsIgnoreCase("ja")));
        values.put("strength5", new BooleanValue(uservalues.get("strength5").equalsIgnoreCase("ja")));
        values.put("strength6", new BooleanValue(uservalues.get("strength6").equalsIgnoreCase("ja")));
        values.put("weakness1", new BooleanValue(uservalues.get("weakness1").equalsIgnoreCase("ja")));
        values.put("weakness2", new BooleanValue(uservalues.get("weakness2").equalsIgnoreCase("ja")));
        values.put("weakness3", new BooleanValue(uservalues.get("weakness3").equalsIgnoreCase("ja")));
        values.put("weakness4", new BooleanValue(uservalues.get("weakness4").equalsIgnoreCase("ja")));
        values.put("weakness5", new BooleanValue(uservalues.get("weakness5").equalsIgnoreCase("ja")));
        values.put("weakness6", new BooleanValue(uservalues.get("weakness6").equalsIgnoreCase("ja")));

        try {
            StructValue struct = new StructValue(values);
            mProject.setVariable("userdata", struct);
        } catch (Exception e) {
            // System.out.println("not running");
        }

        // Hide the questionnaire window
        mQuestionnaireGUI.setVisible(false);

        // End the Activity Worker
        synchronized (mActivityWorker) {
            mActivityWorkers.clear();
            mActivityWorker.notifyAll();
        }
    }
}
