package de.dfki.vsm.xtension.InteractionActProducer;

import com.google.gson.Gson;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.consumer.*;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;

import java.lang.reflect.Type;
import java.time.Duration;
import java.util.Arrays;
import java.util.Properties;

public class KafkaHandler extends Thread{
    private KafkaProducer<String, String> producer;
    private Consumer<Long, String> consumer;

    private final InteractionActProducerExecutor executor;
    private final String[] topics;
    private final String server;
    private boolean stop = false;
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    private Gson gson = new Gson();
    private Type jsonClass;

    public KafkaHandler(String server, String topics, InteractionActProducerExecutor executor) {
        logger.message("KafkaHandler COnstructor: " + server + " " + topics + " ");

        this.server = server;
        this.topics = topics.split(",");
        this.jsonClass = jsonClass;
        this.executor = executor;
    }

    //Start function of the Thread
    //Initialieses the kafka consumer and with super.start() indirectly triggeres the run() method
    @Override
    public synchronized void start() {
        startProducer();
        startConsumer();

        System.out.println("InteractionAct Kafka consumer set up");
        super.start();
    }

    private void startProducer(){
        Properties props = new Properties();
        props.put("bootstrap.servers", server);
        props.put("acks", "all");
        props.put("retries", 0);
        props.put("linger.ms", 1);
        props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        producer = new KafkaProducer<>(props);
    }

    private void startConsumer(){
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
    }

    //This run method whill listen to the in the properties set Kafka Topics
    //I new entries are in the topic, the handle method is called
    @Override
    public void run() {

        Duration duration = Duration.ofMillis(1000);

        logger.message("InteractionAct Kafka consumer starts listening");

        while (!stop) {
            final ConsumerRecords<Long, String> consumerRecords =
                    consumer.poll(duration);

            consumerRecords.forEach(record -> {
                handle(record);
            });
            consumer.commitAsync();
        }
        consumer.close();
    }

    //interface for executor to use kafka produced. Given topic, key and Message, makes new kafka entry
    public void sendToKafka(String topic, String key, String content){
        ProducerRecord<String, String> record = new ProducerRecord<>(topic, 0, key, content);

        logger.message(String.valueOf(record));
        try {
            RecordMetadata metaData = producer.send(record).get();
        } catch (Exception e) {
            logger.failure(e.toString());
            logger.failure(e.getCause().toString());
        }
        producer.flush();
    }

    //This method handles new messages from kafka
    //currently it only detects if it is a message from the STT topic and sends the content to the executor
    //Note: Further implementation required which casts the STT gson to the proper Java object
    private void handle(ConsumerRecord<Long, String> record) {
        //logger.message(record.value().toString());
        //When reimplementing casting to jason remember try and catch for mJsonSyntaxException

        switch (record.topic()) {
            case "STT":
                executor.handleSTT(record.value());
                break;


        }

    }

    //Used to unloading the object, stops the thread listening to Kafka
    public final void abort() {

        stop = true;
//        while (stop) {
//            consumer.close();
//        interrupt();
//                break;
//        }
    }
}
