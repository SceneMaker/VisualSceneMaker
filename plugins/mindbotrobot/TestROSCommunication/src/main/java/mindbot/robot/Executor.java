package mindbot.robot;

import org.ros.address.InetAddressFactory;
import org.ros.node.DefaultNodeMainExecutor;
import org.ros.node.NodeConfiguration;
import org.ros.node.NodeMainExecutor;

import java.net.URI;

// This class will run a publisher or a subscriber
public class Executor {



    public static void main(String[] argv) throws Exception {

        boolean runSubscriber = false ;
        boolean runClient = true ;

        if(argv.length < 1) {
            System.err.println("Missing <ROSCORE_URL> argument, e.g.: 'http://localhost:11311'");
            System.exit(10);
        }

        String ros_url = argv[0] ;
        ros_url = "http://localhost:11311";
        URI ros_uri = URI.create(ros_url) ;

        // MindbotPublisher pubNodeMain;
        MindbotSubscriber subNodeMain;
        MindbotClient clientNodeMain;
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
            String host = InetAddressFactory.newNonLoopback().getHostAddress();
            NodeConfiguration subNodeConfiguration = NodeConfiguration.newPublic(host, ros_uri);
            subNodeMain = new MindbotSubscriber();
            subNodeConfiguration.setNodeName("MindbotSubscriber");
            nodeMainExecutor.execute(subNodeMain, subNodeConfiguration);
            // nodeMainExecutor.shutdownNodeMain(subNodeMain);
            // nodeMainExecutor.shutdown();
        }

        // Execute the Client
        if (runClient) {
            String host = InetAddressFactory.newNonLoopback().getHostAddress();
            NodeConfiguration clientNodeConfiguration = NodeConfiguration.newPublic(host, ros_uri);
            clientNodeMain = new MindbotClient();
            clientNodeConfiguration.setNodeName("MindbotClient");
            nodeMainExecutor.execute(clientNodeMain, clientNodeConfiguration);
            clientNodeMain.callClient();
            // nodeMainExecutor.shutdownNodeMain(subNodeMain);
            // nodeMainExecutor.shutdown();
        }
    }
}