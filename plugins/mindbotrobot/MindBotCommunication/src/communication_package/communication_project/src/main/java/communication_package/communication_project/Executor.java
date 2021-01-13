package communication_package.communication_project;

import org.ros.address.InetAddressFactory;
import org.ros.node.DefaultNodeMainExecutor;
import org.ros.node.NodeConfiguration;
import org.ros.node.NodeMainExecutor;

import java.util.Arrays;
import java.net.URI;

// This class will run a publisher or a subscriber
public class Executor {

    public static void main(String[] argv) throws Exception {

        communication_package.communication_project.MindbotPublisher pubNodeMain;
        communication_package.communication_project.MindbotSubscriber subNodeMain;
        NodeMainExecutor nodeMainExecutor = DefaultNodeMainExecutor.newDefault();

        // Execute the Publisher
        String pubsArgv = "communication_package.communication_project.MindbotPublisher";
        if (Arrays.asList(argv).contains(pubsArgv)) {
            String host = InetAddressFactory.newNonLoopback().getHostAddress();
            NodeConfiguration pubNodeConfiguration = NodeConfiguration.newPublic(host,
                    URI.create("http://localhost:11311"));
            pubNodeMain = new communication_package.communication_project.MindbotPublisher();
            pubNodeConfiguration.setNodeName("MindbotPublisher");
            nodeMainExecutor.execute(pubNodeMain, pubNodeConfiguration);
            // nodeMainExecutor.shutdownNodeMain(pubNodeMain);
            // nodeMainExecutor.shutdown();
        }

        // Execute the Subscriber
        String subsArgv = "communication_package.communication_project.MindbotSubscriber";
        if (Arrays.asList(argv).contains(subsArgv)) {
            String host = InetAddressFactory.newNonLoopback().getHostAddress();
            NodeConfiguration subNodeConfiguration = NodeConfiguration.newPublic(host,
                    URI.create("http://localhost:11311"));
            subNodeMain = new communication_package.communication_project.MindbotSubscriber();
            subNodeConfiguration.setNodeName("MindbotSubscriber");
            nodeMainExecutor.execute(subNodeMain, subNodeConfiguration);
            // nodeMainExecutor.shutdownNodeMain(subNodeMain);
            // nodeMainExecutor.shutdown();
        }
    }
}