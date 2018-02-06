package de.dfki.vsm.xtension.decad;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;
import de.dfki.vsm.xtension.decad.factories.DecadCommandFactory;
import de.dfki.vsm.xtension.decad.utils.DECADLongPoller;

import java.io.IOException;
import java.util.concurrent.Semaphore;


public class DecadExecutor extends ActivityExecutor{

    private static final String DECAD_MARKER_SEPARATOR = "$";
    protected DecadCommandFactory factory;
    protected final Semaphore speechSemaphore;
    private final DECADLongPoller poller;

    public DecadExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        factory = new DecadCommandFactory();
        speechSemaphore = new Semaphore(1);
        poller = new DECADLongPoller();
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
            e.printStackTrace();
        }
    }

    private void executeCommandFor(AbstractActivity activity) throws IOException, InterruptedException {
        if (activity instanceof SpeechActivity) {
            speechSemaphore.acquire();
            poller.pollIsSpeaking(this);
        }
        DecadCommand command = factory.getCommand(activity);
        command.execute();
    }

    @Override
    public void launch() {

    }

    @Override
    public void unload() {

    }

    public void handle() {
        speechSemaphore.release();
    }


}
