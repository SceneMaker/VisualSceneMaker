package de.dfki.vsm.xtension.mithos;

import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.consumer.Consumer;

import java.time.Duration;
import java.util.Properties;


public class MithosHandler extends Thread{

    private final String topic;
    private final String server;
    private final Consumer<Long, String> consumer;
    private boolean stop = false;

    public MithosHandler(String server, String topic) {
        this.server = server;
        this.topic = topic;

        Properties props = new Properties();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, this.server);
        props.put(ConsumerConfig.GROUP_ID_CONFIG, "VSMConsumer");
        props.put("retries", 0);
        props.put("linger.ms", 1);
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer");
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer");

        consumer = new KafkaConsumer<>(props);
    }

    @Override
    public void run() {

        Duration duration = Duration.ofMillis(1000);

        while(!stop){
            final ConsumerRecords<Long, String> consumerRecords =
                    consumer.poll(duration);

            consumerRecords.forEach(record -> {
                System.out.printf("Consumer Record:(%d, %s, %d, %d)\n",
                        record.key(), record.value(),
                        record.partition(), record.offset());
            });

            consumer.commitAsync();
        }
        consumer.close();
    }


    public void stopListening(){
        this.stop=false;
    }
}
