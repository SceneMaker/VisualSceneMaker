/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.InteractionActProducer;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;




/**
 *
 * @author Alexander Haberl
 */


public class InteractionActProducerExecutor extends ActivityExecutor {
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private KafkaHandler handler;

    private final String server;
    private final String read_topics;

    private final String write_topic;

    //Constructor, leads properties defined in VCM project settings for plugin
    public InteractionActProducerExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);

        server = mConfig.getProperty("server");
        read_topics = mConfig.getProperty("read_topic");
        write_topic = mConfig.getProperty("write_topic");
    }




    //Launch Method starts the KafkaHandler
    @Override
    public void launch() {

        mLogger.message("InteractionActProducer launched");

        handler = new KafkaHandler(server, read_topics, this);
        handler.start();
    }

    //Method called by VSM on the ALMA VCM agent. This function mostly delegates to the corresponding subfunction to a given activity
    @Override
    public void execute(AbstractActivity activity) {

        mLogger.message("InteractionActProducer activity called from VCM. NOTHING IMPLEMENTED YET");
    }

    //Method is called when KafkaHandler resieves a new STT entry
    //Currently Dummy FUnction, just writes dummy to InteractionAct topic
    public void handleSTT(String content){

        mLogger.message("Executor reseived STT: " + content);

        //TODO add logic here

        handler.sendToKafka(write_topic, "testDummy", "Test Content");
    }
    @Override
    public void unload() {
        handler.abort();
    }

    @Override
    public synchronized String marker(long id) {
        return "$(" + id + ")";
    }


}
