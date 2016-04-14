package de.dfki.vsm.xtension.stickmanmarytts.util.tts;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.stickmanmarytts.StickmanMaryttsExecutor;
import marytts.server.Mary;
import marytts.server.MaryProperties;

import java.io.IOException;
import java.net.*;

/**
 * Created by alvaro on 4/11/16.
 */
public class I4GMaryServer  {
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The executor instance

    // The local socket port
    private  int mPort;


    private  static I4GMaryServer mInstance;

    public I4GMaryServer(final int port, final PluginConfig config) {
        // Initialize the port
        mPort = port;
    }



    public I4GMaryServer(final PluginConfig config) {
        Runnable main1 = null;
        //Setting up everything
        // Get the plugin configuration
        String maryBase = config.getProperty("mary.base");
        if(maryBase != null && !maryBase.equals("")){
            System.setProperty("mary.base", maryBase);
        }

        int main = MaryProperties.needInteger("socket.port");
        try {
            ServerSocket e = new ServerSocket(main);
            e.close();
        } catch (IOException var6) {
            System.err.println("\nPort " + main + " already in use!");

        }

        try {
            Mary.startup();
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            main1 = (Runnable)Class.forName("marytts.server.http.MaryHttpServer").newInstance();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        new Thread(main1).start();
        //main1.run();
    }

    public static I4GMaryServer getInstance(final PluginConfig config){
        if(mInstance == null ){
            mInstance = new I4GMaryServer(config);
        }
        return mInstance;
    }


}
