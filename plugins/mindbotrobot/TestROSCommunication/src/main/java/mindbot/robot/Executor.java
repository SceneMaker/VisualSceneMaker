package mindbot.robot;

import org.ros.address.InetAddressFactory;
import org.ros.node.DefaultNodeMainExecutor;
import org.ros.node.NodeConfiguration;
import org.ros.node.NodeMainExecutor;

import java.net.URI;

// This class will run a publisher or a subscriber
public class Executor {



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


        // MindbotPublisher pubNodeMain;
        MindbotTopicReceiver topicReceiver;
        MindbotServiceRequester serviceReq;
        NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();

        // Execute the Publisher
        /*
        String pubsArgv = "communication_package.communication_project.MindbotPublisher";
        if (Arrays.asList(argv).contains(pubsArgv)) {
            String host = InetAddressFactory.newNonLoopback().getHostAddress();
            NodeConfiguration pubNodeConfiguration = NodeConfiguration.newPublic(host, ros_uri);
            pubNodeMain = new MindbotPublisher();
            pubNodeConfiguration.setNodeName("MindbotPublisher");
            nodeMainExecutor.execute(pubNodeMain, pubNodeConfiguration);
            // nodeMainExecutor.shutdownNodeMain(pubNodeMain);
            // nodeMainExecutor.shutdown();
        }
        */

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
            serviceReq = new MindbotServiceRequester();
            clientNodeConfiguration.setNodeName("MindbotServiceRequester");

            nodeMainExecutor.execute(serviceReq, clientNodeConfiguration);
            try { Thread.sleep(2000); } catch (Exception e) {} // wait for the node setUpClient to be executed.

            //clientNodeMain.setUpClient(clientNodeConfiguration);
            //clientNodeMain.setUpClient(nodeMainExecutor) ;
            serviceReq.setTcpTarget(0.1f, 0.2f, 1, 1, 0, 0, 0);
            // nodeMainExecutor.shutdownNodeMain(subNodeMain);
            // nodeMainExecutor.shutdown();
        }
    }
}