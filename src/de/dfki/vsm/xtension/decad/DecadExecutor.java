package de.dfki.vsm.xtension.decad;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableCompletion;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;
import de.dfki.vsm.xtension.decad.factories.DecadCommandFactory;
import de.dfki.vsm.xtension.decad.properties.ExportAnimations;
import de.dfki.vsm.xtension.decad.properties.ExportProperties;
import de.dfki.vsm.xtension.decad.utils.SpeechSynchronizer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;


public class DecadExecutor extends ActivityExecutor implements ExportableProperties, ExportableCompletion {

    private static final String DECAD_MARKER_SEPARATOR = "$";
    private final SpeechSynchronizer speechSynchronizer;
    protected DecadCommandFactory factory;
    private ExportAnimations animationsToExport;
    private ExportProperties exportProperties;

    public DecadExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        Constants.buildURL(config.getProperty("url"), config.getProperty("port"));
        factory = new DecadCommandFactory();
        speechSynchronizer = new SpeechSynchronizer();
        animationsToExport = new ExportAnimations();
        exportProperties = new ExportProperties();

    }


    @Override
    public String marker(long id) {
        return DECAD_MARKER_SEPARATOR + id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        try {
            executeCommandFor(activity);
        } catch (IOException | InterruptedException e) {
            mLogger.failure(e.getMessage());
        }
    }

    private void executeCommandFor(AbstractActivity activity) throws IOException, InterruptedException {
        speechSynchronizer.snchronizeSpeech(activity);
        DecadCommand command = factory.getCommand(activity);
        command.execute();
    }

    @Override
    public void launch() {
        //Make in the class BrowserOpener.openBrowserAt to wait until the webpage is loaded
    }


    @Override
    public void unload() {
        //Cannot close it cause it is now handled by the OS
    }


    @Override
    public ArrayList<String> getExportableActions() {
        return animationsToExport.getExportableActions();
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportProperties.getExportableProperties();
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return exportProperties.getExportableAgentProperties();
    }
}
