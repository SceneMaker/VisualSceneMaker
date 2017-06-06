package de.dfki.vsm.xtension.remotesender;

import de.dfki.vsm.editor.project.EditorProject;
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
import de.dfki.vsm.xtension.remotesender.senders.StringDefaultSender;
import de.dfki.vsm.xtension.remotesender.senders.uiavatarsender.SpeechRecognitionSender;

import java.io.*;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Created by alvaro on 5/2/17.
 */
public class RemoteSenderExecutor extends ActivityExecutor implements ExportableProperties{
    private  SpeechRecognitionSender speechRecognitionSender;
    private Clientable client;
    private ClientsFactory clientsFactory;
    private SenderTypeFactory senderTypeFactory;
    private ExportableProperties exportableProperties = new RemoteSenderProjectProperty();
    private String clientId;

    public RemoteSenderExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        clientsFactory = new ClientsFactory(mConfig);
        senderTypeFactory = new SenderTypeFactory();

    }

    @Override
    public void launch() {
        try {
            startUIServer();
            startClient();
            int a= 0;
        }  catch (InterruptedException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void startUIServer() {
        JarRunner jarRunner = new JarRunner("/home/alvaro/Documents/WorkHiwi/AvatarControl/out/artifacts/AvatarControl_jar/AvatarControl.jar");
        jarRunner.run();

    }

    private void startClient() throws InterruptedException, IOException {
        client = clientsFactory.buildClient();
        boolean connected = false;
        tryToConnectToServer(connected);
        BufferedReader input = new BufferedReader(new InputStreamReader(client.getInputStream()));
        clientId = input.readLine();
        System.out.println(clientId);
        speechRecognitionSender = new SpeechRecognitionSender(client);
        senderTypeFactory.setSpeechRecognitionSender(speechRecognitionSender);
    }

    private void tryToConnectToServer(boolean connected) throws InterruptedException {
        while (!connected){
            try {
                client.connect();
            } catch (IOException e) {
                continue;
            }
            connected = client.isConnected();
            Thread.sleep(200);
        }
    }

    @Override
    public void unload() {
        DataSendable closeSender = new StringDefaultSender("QUIT " + clientId);
        client.setDataCreator(closeSender);
        try {
            client.send();
        } catch (IOException e) {
            e.printStackTrace();
        }
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
        if(message.startsWith("$")){
            String variable = message.substring(1);
            message = String.valueOf(mProject.getValueOf(variable).getValue());
        }
        String separator = clip(activity.get("separator"));
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
        String text = activity.getTextOnly("$(").trim();
        LinkedList<String> timemarks = activity.getTimeMarks("$(");
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

    private class JarRunner {
        private final String filename;
        private final File file;

        public JarRunner(String filename){
            this.filename = filename;
            this.file = new File(filename);
        }
        public  void run() {
            ProcessBuilder pb = new ProcessBuilder("java", "-jar", this.filename);
            pb.directory(new File(file.getParent()));
            try {
                Process p = pb.start();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    class LogStreamReader implements Runnable {

        private BufferedReader reader;
        public LogStreamReader(InputStream is) {
            this.reader = new BufferedReader(new InputStreamReader(is));
        }

        public void run() {
            try {
                String line = reader.readLine();
                while (line != null) {
                    System.out.println(line);
                    line = reader.readLine();
                }
                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
