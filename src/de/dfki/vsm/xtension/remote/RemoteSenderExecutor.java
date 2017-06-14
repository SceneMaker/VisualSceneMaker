package de.dfki.vsm.xtension.remote;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.xtension.remote.client.factories.ClientsFactory;
import de.dfki.vsm.xtension.remote.client.factories.remoteagent.RemoteAgentAbstractFactory;
import de.dfki.vsm.xtension.remote.client.properties.RemoteSenderProjectProperty;
import de.dfki.vsm.xtension.remote.client.sender.Clientable;
import de.dfki.vsm.xtension.remote.client.sender.DataSendable;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 5/2/17.
 */
public class RemoteSenderExecutor extends ActivityExecutor implements ExportableProperties{
    private Clientable client;
    private ClientsFactory clientsFactory;
    private ExportableProperties exportableProperties = new RemoteSenderProjectProperty();

    public RemoteSenderExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        clientsFactory = new ClientsFactory(mConfig);
    }

    @Override
    public void launch() {
        try {
            startClient();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void startClient() throws IOException {
        client = clientsFactory.buildClient();
        client.connect();
    }

    @Override
    public void unload() {

    }

    @Override
    public String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            exectureSpeechActivity((SpeechActivity) activity);
        } else {
            prepareDataAndSend(activity);
        }
    }

    private void prepareDataAndSend(AbstractActivity activity) {
        DataSendable sendable = buildSendable(activity);
        client.setDataCreator(sendable);
        send();
    }

    private void send() {
        try {
            client.send();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private DataSendable buildSendable(AbstractActivity activity) {
        AgentConfig agentConfiguration = mProject.getAgentConfig(activity.getActor());
        return RemoteAgentAbstractFactory.createRemoteAgent(agentConfiguration, activity).getSendable();
    }

    private void exectureSpeechActivity(SpeechActivity activity) {
        String text = activity.getTextOnly("$(").trim();
        LinkedList<String> timemarks = activity.getTimeMarks("$(");
        if (text.isEmpty()) {
            ExecuteActivityAtTimeMark(timemarks);
        }else{
            prepareDataAndSend(activity);
        }
    }

    private void ExecuteActivityAtTimeMark(LinkedList<String> timemarks) {
        for (String tm : timemarks) {
            mLogger.warning("Directly executing activity at timemark " + tm);
            mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
        }
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties.getExportableProperties();
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}