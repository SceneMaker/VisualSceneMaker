package de.dfki.vsm.xtension.mithos;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.mithos.compint.command.ScenarioScriptFeedback;
import de.mithos.compint.interaction.InteractionAct;
import org.apache.kafka.clients.consumer.*;

import java.lang.reflect.Type;
import java.time.Duration;
import java.util.Arrays;
import java.util.Properties;

/**
 * This plugin uses a kafka server to control an agent-environment and to receive processed userdata.
 * kafka consumer implementation based on: https://dzone.com/articles/writing-a-kafka-consumer-in-java
 *
 * @author Manuel Anglet
 */

public class MithosHandler<T> extends Thread {

    private final String[] topics;
    private final String server;
    private Consumer<Long, String> consumer;
    private boolean stop = false;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private Gson gson = new Gson();
    private Type jsonClass;

    public MithosHandler(String server, String topics) {
        this.server = server;
        this.topics = topics.split(",");
        this.jsonClass = jsonClass;
    }

    @Override
    public synchronized void start() {
        Properties props = new Properties();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, this.server);
        props.put(ConsumerConfig.GROUP_ID_CONFIG, "VSMConsumer");
        props.put("retries", 0);
        props.put("linger.ms", 1);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer");
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer");

        consumer = new KafkaConsumer<>(props);
        consumer.subscribe(Arrays.asList(topics));
        System.out.println("Mithos Kafka consumer set up");
        super.start();
    }

    @Override
    public void run() {

        Duration duration = Duration.ofMillis(1000);

        System.out.println("Mithos Kafka consumer starts listening");
        mLogger.message("Mithos Kafka consumer starts listening");

        while(!stop){
            final ConsumerRecords<Long, String> consumerRecords =
                    consumer.poll(duration);

            consumerRecords.forEach(record -> {
                handle(record);
            });
            consumer.commitAsync();
        }
    }

    private void handle(ConsumerRecord<Long, String> record) {
        mLogger.message(record.toString());
        try {
            switch (record.topic()) {
                case "SSF":
                    ScenarioScriptFeedback ssf = gson.fromJson(record.value(), ScenarioScriptFeedback.class);
                case "InteractionActs":
                    InteractionAct ia = gson.fromJson(record.value(), InteractionAct.class);

            }
        } catch (JsonSyntaxException jse) {
            mLogger.failure(jse.toString());
        }
    }

    public final void abort() {
        stop = true;
        //consumer.close();
        interrupt();
    }

    private void handle(InteractionAct intAct) {

    }
}
