package de.dfki.vsm.xtension.mindbotrobot;

import org.ros.address.InetAddressFactory;
import org.ros.node.DefaultNodeMainExecutor;
import org.ros.node.NodeConfiguration;
import org.ros.node.NodeMainExecutor;

import java.net.URI;
import java.util.LinkedList;
import java.util.List;

// This class will run a publisher or a subscriber
public class StandaloneExecutor {

    public static void main(String[] argv) throws Exception {

        boolean runSubscriber = true ;
        boolean runClient = true ;

        if(argv.length < 1) {
            System.err.println("Missing <ROSCORE_URL> argument, e.g.: 'http://localhost:11311'");
            System.exit(10);
        }

        String ros_url = argv[0] ;
        //ros_url = "http://localhost:11311";
        URI ros_uri = URI.create(ros_url) ;

        // The local IP/port host use to contact ROS
        String host = InetAddressFactory.newNonLoopback().getHostAddress();


        MindbotTopicReceiver topicReceiver;
        MindbotServiceRequester serviceReq;
        NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();


        // Execute the Subscriber
        if (runSubscriber) {
            NodeConfiguration subNodeConfiguration = NodeConfiguration.newPublic(host, ros_uri);
            topicReceiver = new MindbotTopicReceiver();
            subNodeConfiguration.setNodeName("MindbotTopicSubscriber");

            nodeMainExecutor.execute(topicReceiver, subNodeConfiguration);
            // nodeMainExecutor.shutdownNodeMain(subNodeMain);
            // nodeMainExecutor.shutdown();
        }

        // Execute the Client
        if (runClient) {
            NodeConfiguration clientNodeConfiguration = NodeConfiguration.newPublic(host, ros_uri);
            serviceReq = new MindbotServiceRequester(null);
            clientNodeConfiguration.setNodeName("MindbotServiceRequester");

            nodeMainExecutor.execute(serviceReq, clientNodeConfiguration);
            try { Thread.sleep(2000); } catch (Exception e) {} // wait for the node setUpClient to be executed.

            serviceReq.setTcpTarget(0.1f, 0.2f, 0.4f, 1, 0, 0, 0);
            List<String> names = new LinkedList<String>() ; names.add("joint1") ;
            double[] p = {1d}; double[] v = {1d}; double[] e = {1d};
            serviceReq.setJointTarget(names, p, v, e);
            serviceReq.setMaxTcpVelocity(1d,1d,1d);
            serviceReq.setMaxTcpAcceleration(1d,1d,1d);
            byte state = 1;
            serviceReq.setCtrlState(state);
            byte mode = 1;
            serviceReq.setCtrlMode(mode);

            // nodeMainExecutor.shutdownNodeMain(subNodeMain);
            // nodeMainExecutor.shutdown();
        }
    }
}