/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.manager.ActivityScheduler;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import javax.swing.SwingUtilities;

/**
 *
 * @author Patrick Gebhard
 */
public class QuestionnaireExecutor extends ActivityExecutor {

    // The GUI
    QuestionnaireGUI mQuestionnaireGUI;
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
    public void execute(AbstractActivity activity, ActivityScheduler player) {
        mLogger.message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
        if (activity instanceof ActionActivity) {
             final String name = activity.getName();
             
             if (name.equalsIgnoreCase("show")) {
                 mQuestionnaireGUI.init();
                 mQuestionnaireGUI.setVisible(true);
             }

             if (name.equalsIgnoreCase("hide")) {
                 mQuestionnaireGUI.setVisible(false);
             }

        }
    }

    @Override
    public void launch() {
        mQuestionnaireGUI = new QuestionnaireGUI();
        SwingUtilities.invokeLater(() -> mQuestionnaireGUI.init());
        
    }

    @Override
    public void unload() {
        // nothing
    }
    
}
