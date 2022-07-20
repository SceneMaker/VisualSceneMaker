package de.dfki.vsm.xtension.mithos;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.consumer.Consumer;

import java.io.IOException;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.Properties;

/**
 * This plugin uses a kafka server to control an agent-environment and to receive processed userdata.
 * kafka consumer implementation based on: https://dzone.com/articles/writing-a-kafka-consumer-in-java
 *
 * @author Manuel Anglet
 */

public class MithosHandler extends Thread{

    private final String topic;
    private final String server;
    private  Consumer<Long, String> consumer;
    private boolean stop = false;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public MithosHandler(String server, String topic) {
        this.server = server;
        this.topic = topic;
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
        consumer.subscribe(Arrays.asList(topic.split(",")));
        System.out.println("Mithos Kafka consumer set up");
        super.start();
    }

    @Override
    public void run() {

        Duration duration = Duration.ofMillis(1000);

        System.out.println("Mithos Kafka consumer starts listening");

        while(!stop){
            final ConsumerRecords<Long, String> consumerRecords =
                    consumer.poll(duration);

            consumerRecords.forEach(record -> {
                mLogger.message(record.toString());
            });

            consumer.commitAsync();
        }
    }

    public final void abort() {
        stop = true;
        consumer.close();
        interrupt();
    }
}
