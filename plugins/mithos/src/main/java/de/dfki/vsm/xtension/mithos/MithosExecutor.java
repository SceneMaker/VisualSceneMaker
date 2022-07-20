package de.dfki.vsm.xtension.mithos;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import de.dfki.vsm.util.WordMapping;

import java.util.List;
import java.util.Properties;

/**
 * This plugin uses a kafka server to control an agent-environment and to receive processed userdata.
 * kafka producer implementation based on mithos semvox producer demo
 *
 * @author Manuel Anglet
 */

public class MithosExecutor extends ActivityExecutor {

    private final String server;
    private final String topic;
    KafkaProducer<String, String> producer;
    MithosHandler handler;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private WordMapping mWordMapping;

    public MithosExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        System.out.println("Mithos Kafka starting");
        server = mConfig.getProperty("server");
        topic = mConfig.getProperty("topic");
    }

    @Override
    public String marker(long id) {
        // Bracket style bookmarks
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {
        System.out.println("Mithos Kafka action to be executed");
        String actor = activity.getActor();
        String command = activity.getText();

        if (activity instanceof SpeechActivity) {
            final SpeechActivity speech_activity = (SpeechActivity) activity;
            final String speech_text = speech_activity.getTextOnly("$(").trim();
            final List<String> time_marks = speech_activity.getTimeMarks("$(");

            if (speech_text.isEmpty()) {
                for (final String tm : time_marks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                    return;
                }
            } else {
                mWordMapping.load(actor, mProject);
                speech_activity.doPronounciationMapping(mWordMapping);
                String aid = mProject.getAgentConfig(actor).getProperty("aid");


            }

            producer.send(new ProducerRecord<String,String>(actor, "speech", "text="+speech_activity.getText()));

        } else {
            producer.send(new ProducerRecord<String, String>(actor, "key_test", command));
        }
    }

    ProducerRecord<String,String> formatSpeechActivity(SpeechActivity speechActivity){
        String text = speechActivity.getText();
        String command;
        List<String> time_marks = speechActivity.getTimeMarks("$(");
        for(String mark: time_marks){

        }
    }



    @Override
    public void launch() {

        Properties props = new Properties();
        props.put("bootstrap.servers", server);
        props.put("acks", "all");
        props.put("retries", 0);
        props.put("linger.ms", 1);
        props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        producer = new KafkaProducer<>(props);
        System.out.println("Mithos Kafka producer set up");

        handler = new MithosHandler(server, topic);
        handler.start();
    }

    @Override
    public void unload() {
        producer.close();
        handler.abort();
    }
}