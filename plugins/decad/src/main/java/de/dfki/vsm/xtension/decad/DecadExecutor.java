package de.dfki.vsm.xtension.decad;

import de.dfki.vsm.model.project.property.ExportableCompletion;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;
import de.dfki.vsm.xtension.decad.factories.DecadCommandFactory;
import de.dfki.vsm.xtension.decad.properties.ExportAnimations;
import de.dfki.vsm.xtension.decad.utils.SpeechSynchronizer;
import de.dfki.vsm.xtension.decad.utils.constants.Constants;

import java.io.IOException;
import java.util.List;


public class DecadExecutor extends ActivityExecutor implements ExportableCompletion {

    private static final String DECAD_MARKER_SEPARATOR = "$";
    private final SpeechSynchronizer speechSynchronizer;
    private final ExportAnimations animationsToExport;
    protected DecadCommandFactory factory;

    public DecadExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        Constants.buildURL(config.getProperty("url"), config.getProperty("port"));
        factory = new DecadCommandFactory();
        speechSynchronizer = new SpeechSynchronizer();
        animationsToExport = new ExportAnimations();
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
        speechSynchronizer.synchronizeSpeech(activity);
        DecadCommand command = factory.getCommand(activity);
        command.execute();
    }

    @Override
    public void launch() {
        //Make in the class BrowserOpener.openBrowserAt to wait until the web page is loaded
    }


    @Override
    public void unload() {
        //Cannot close it cause it is now handled by the OS
    }


    @Override
    public List<String> getExportableActions() {
        return animationsToExport.getExportableActions();
    }

}
