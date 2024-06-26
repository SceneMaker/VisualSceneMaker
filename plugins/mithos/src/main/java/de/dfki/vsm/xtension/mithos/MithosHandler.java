package de.dfki.vsm.xtension.mithos;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.mithos.compint.command.ScenarioScriptFeedback;
import de.mithos.compint.interaction.*;
import org.apache.kafka.clients.consumer.*;

import java.lang.reflect.Type;
import java.time.Duration;
import java.util.*;

/**
 * This plugin uses a kafka server to control an agent-environment and to receive processed userdata.
 * kafka consumer implementation based on: https://dzone.com/articles/writing-a-kafka-consumer-in-java
 *
 * @author Manuel Anglet
 */

public class MithosHandler<T> extends Thread {


    private final MithosExecutor executor;
    private final String[] topics;
    private final String server;
    private Consumer<Long, String> consumer;
    private boolean stop = false;
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    private Gson gson = new Gson();
    private Type jsonClass;

    public MithosHandler(String server, String topics, MithosExecutor executor) {
        this.server = server;
        this.topics = topics.split(",");
        this.jsonClass = jsonClass;
        this.executor = executor;
    }

    @Override
    public synchronized void start() {
        Properties props = new Properties();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, this.server);
        props.put(ConsumerConfig.GROUP_ID_CONFIG, "VSMConsumer");
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer");
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer");
        props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true");
        props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "1");
//        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest");
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        props.put(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "1000");
        consumer = new KafkaConsumer<>(props);
        consumer.subscribe(Arrays.asList(topics));
        System.out.println("Mithos Kafka consumer set up");
        super.start();
    }

    @Override
    public void run() {

        Duration duration = Duration.ofMillis(1000);

//        System.out.println("Mithos Kafka consumer starts listening");
        logger.message("Mithos Kafka consumer starts listening");

        while (!stop) {
            final ConsumerRecords<Long, String> consumerRecords =
                    consumer.poll(duration);

//            TODO: fix error on stopping Project. Catch InterruptException?


            consumerRecords.forEach(record -> {
                handle(record);
            });
            consumer.commitAsync();
        }
        consumer.close();
    }

    private void handle(ConsumerRecord<Long, String> record) {
        //logger.message(record.value().toString());
        try {
            switch (record.topic()) {
                case "SSF":
                    logger.message("SSF");
                    ScenarioScriptFeedback ssf = gson.fromJson(record.value(), ScenarioScriptFeedback.class);
                    executor.process(ssf);
                    logger.message(record.value());
                    break;
                /*                 */
                case "InteractionActs":
                    //TODO DELETE
                    logger.message("IA");
                    logger.message("InteractionAct json: " + record.value());
                    InteractionAct ia = gson.fromJson(record.value(), InteractionAct.class);

                    executor.process(ia);
                    logger.message("InteractionActs");
                    break;
                case "PAD":
                    //Note line above does not work like in the other cases, because we don't have timeBegin and timeEnd of the Emotion CLass
                    Emotion emotion = gson.fromJson(record.value().replace('"',' ').replace("pleasure", "valence"), Emotion.class);//TODO probably time is missing in record.value
                    executor.process(emotion);
                    break;

            }
        } catch (JsonSyntaxException jse) {
            logger.failure(jse.toString());
        }
    }

    public final void abort() {

        stop = true;
//        while (stop) {
//            consumer.close();
//        interrupt();
//                break;
//        }
    }

//    private void handle(InteractionAct intAct) {
//
//    }
}
