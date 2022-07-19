package de.dfki.vsm.xtension.mithos;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import java.util.Properties;

/**
 * This plugin uses a kafka server to control an agent-environment and to receive processed userdata.
 * kafka implementation based on: https://dzone.com/articles/writing-a-kafka-consumer-in-java
 *
 * @author Manuel Anglet
 */

public class MithosExecutor extends ActivityExecutor {

    private final String server;
    private final String topic;
    KafkaProducer<String, String> producer;
    MithosHandler handler;


    public MithosExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);

        server = mConfig.getProperty("server");
        topic = mConfig.getProperty("topic");

        handler = new MithosHandler(server, topic);

        Properties props = new Properties();
        props.put("bootstrap.servers", server);
        props.put("acks", "all");
        props.put("retries", 0);
        props.put("linger.ms", 1);
        props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        KafkaProducer<String, String> producer = new KafkaProducer<>(props);
    }

    @Override
    public String marker(long id) {
        // Bracket style bookmarks
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {
        producer.send(new ProducerRecord<String, String>(topic, "key_test", "Message test"));
    }

    @Override
    public void launch() {
        handler.run();
    }

    @Override
    public void unload() {
        producer.close();
        handler.stopListening();
    }
}