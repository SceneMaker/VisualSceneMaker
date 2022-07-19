package de.dfki.vsm.xtension.mithos;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
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

        producer.send(new ProducerRecord<String, String>(topic, "key_test", "Message test"));
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
        KafkaProducer<String, String> producer = new KafkaProducer<>(props);
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