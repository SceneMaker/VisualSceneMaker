package de.dfki.vsm.xtension.remotesender;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.xtension.remotesender.factories.ClientsFactory;
import de.dfki.vsm.xtension.remotesender.factories.SenderTypeFactory;
import de.dfki.vsm.xtension.remotesender.properties.RemoteSenderProjectProperty;
import de.dfki.vsm.xtension.remotesender.sender.Clientable;
import de.dfki.vsm.xtension.remotesender.sender.DataSendable;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 5/2/17.
 */
public class RemoteSenderExecutor extends ActivityExecutor implements ExportableProperties{
    private Clientable client;
    private ClientsFactory clientsFactory;
    private String separator;
    private SenderTypeFactory senderTypeFactory;
    private ExportableProperties exportableProperties = new RemoteSenderProjectProperty();

    public RemoteSenderExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        clientsFactory = new ClientsFactory(mConfig);
        senderTypeFactory = new SenderTypeFactory();
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
            String blocking = activity.get("blocking");
            DataSendable sendable = buildSendable(activity);
            client.setDataCreator(sendable);
            send();
        }
    }

    private void send() {
        try {
            client.send();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private DataSendable buildSendable(AbstractActivity activity) {
        String message = clip(activity.get("message"));
        separator = clip(activity.get("separator"));
        String senderType = clip(activity.get("type"));
        return senderTypeFactory.buildSendable(senderType, message, separator);
    }

    private String clip(String message) {
        if(message != null){
            return message.replace("'", "");
        }
        return "";
    }

    private void exectureSpeechActivity(SpeechActivity activity) {
        SpeechActivity sa = activity;
        String text = sa.getTextOnly("$(").trim();
        LinkedList<String> timemarks = sa.getTimeMarks("$(");
        if (text.isEmpty()) {
            ExecuteActivityAtTimeMark(timemarks);
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
