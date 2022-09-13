package de.dfki.vsm.xtension.mindbotssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;

import java.io.IOException;
//import java.io.ObjectInputFilter;
import java.net.SocketException;
import java.net.*;

/**
 * This wraps the MindBotSSIPlugin, since we cannot inherit ActivityExecutor in it
 */

public class MindBotSSIExecutor extends ActivityExecutor  {

    DatagramSocket mSocket;
    // InetAddress address ;
    // int port = 1111 ;

    //String hostname="localhost";
    // InetAddress server = InetAddress.getByName(hostname);
    //   this.mSocket = new DatagramSocket(1111,server);

    public MindBotSSIExecutor(PluginConfig config,
                              RunTimeProject project) {
        super(config, project);
        //this.mPlugin = new MindBotSSIPlugin(config, project);
        //  super("Test SSI start invocation");
    }


    @Override
    public String marker(long id) {
        return "$" + id;
    }


    public void execute(AbstractActivity activity) {

        mLogger.message("PRINT"+ activity);
        if(activity.getName().equals("start_recording")) {
            this.sendStart();
        }
        else if (activity.getName().equals("stop_recording")) {
            this.sendStop();
        }
    }



    @Override
    public void launch() {

        String hostname = "localhost";
        int port = 1111;
        mLogger.message("Initializing Datagram connection to host " + hostname + " for port " + port);

        try {
            InetAddress server = InetAddress.getByName(hostname);
            this.mSocket = new DatagramSocket(1111, server);
            //this.mSocket.connect(this.address,this.port);
            this.mSocket.connect(server, port);
        } catch(UnknownHostException | SocketException e) {
            mLogger.failure(e.toString());
        }

    }



    @Override
    public void unload() {
        mLogger.message("unloading");

        // this.sendStop();
        mSocket.close();
    }


    public void sendStart() {
        this.sendBytes(
                new byte[]{0X05, 0X00, 0X00, 0X00}
        );
        this.sendBytes(
                new byte[]{0X00, 0X00, 0X00, 0X00, 0X65, 0X6e, 0X61, 0X62, 0X6c, 0X65, 0X00}
        );
        this.sendBytes(
                new byte[]{0X06, 0X00, 0X00, 0X00}
        );
    }


    public void sendStop() {
        this.sendBytes(
                new byte[]{0X02, 0X00, 0X00, 0X00}
        );
        this.sendBytes(
                new byte[]{0X00, 0X00, 0X00, 0X00, 0X64, 0X69, 0X73, 0X61, 0X62, 0X6c, 0X65, 0X00}
        );
        this.sendBytes(
                new byte[]{0X03, 0X00, 0X00, 0X00}
        );
    }


    private void sendBytes(byte[] buffer) {
        try {
            int port  = 1111;
            // String hostname = "localhost";
            // InetAddress server = InetAddress.getByName(hostname);
            // DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
            // byte[] buffer = new byte[65508];
            InetAddress address = InetAddress.getLocalHost();

            DatagramPacket packet = new DatagramPacket(
                    buffer, buffer.length, address, port);
            System.out.print("Sending " + address+" ");
            System.out.print( buffer.length + " bytes..." + "to port" +" "+ port+" ");
            this.mSocket = new DatagramSocket();
            mSocket.send(packet);

            System.out.println("sent.");

        } catch (IOException e) {
            mLogger.warning(e.toString());
        }


    }
    // Handle SSI event array
    // @Override
    private void handle(final SSIEventArray array) {
        // Print some information
        mLogger.message("Got SSI message array of size " + array.size());
        for (final SSIEventEntry event_entry : array.list()) {
            final SSIEventData data = event_entry.getData();
            // Imagine the event was produced from address "coords@mouse" or "click@mouse"
            mLogger.message(" - sender: " +  event_entry.getSender() + // "mouse"
                    "\t event: " + event_entry.getEvent() + // "coords"
                    "\t from: " + event_entry.getFrom() + // an integer number (?)
                    "\t type: " + event_entry.getType() + // TUPLE for coords, or EMPTY for clicks
                    "\t state: " + event_entry.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + data);

            String sender = event_entry.getSender() ;
            String event = event_entry.getEvent() ;

            if (sender.equals("mouse") && event.equals("coords")) {
                assert event_entry.getType().equals("STRING") ;
                String data_str = data.toString() ;
                // Format, e.g.: 0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675
                String[] coords = data_str.split(";") ;
                // Split the last element
                String[] last_coords_str = coords[coords.length - 1].split(",") ;
                assert last_coords_str.length == 2 ;
                float x = Float.parseFloat(last_coords_str[0]) ;
                float y = Float.parseFloat(last_coords_str[1]) ;
                mLogger.message("Most recent Mouse coords: \t" + x + "\t" + y);

                mProject.setVariable("ssi_mouse_x", x);
                mProject.setVariable("ssi_mouse_y", y);

            } else if (sender.equals("mouse") && event.equals("click")) {
                assert event_entry.getType().equals("STRING") ;
                assert data == null ; // Mouse clicks bring no data
                String state = event_entry.getState() ;
                if (state.equals("continued")) {
                    mLogger.message("Mouse Click DOWN");
                } else if (state.equals("completed")) {
                    mLogger.message("Mouse Click UP. Duration: " + event_entry.getDur());
                } else {
                    assert false;
                }
            }

        }
    }

}
