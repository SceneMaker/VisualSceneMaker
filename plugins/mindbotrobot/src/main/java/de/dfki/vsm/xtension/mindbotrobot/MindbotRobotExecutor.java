package de.dfki.vsm.xtension.mindbotrobot;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import geometry_msgs.Point;
import geometry_msgs.Pose;
import geometry_msgs.Quaternion;
import org.ros.node.DefaultNodeMainExecutor;
import org.ros.node.NodeConfiguration;
import org.ros.node.NodeMainExecutor;

import java.net.*;
import java.util.*;
import java.util.stream.Collectors;

public final class MindbotRobotExecutor extends ActivityExecutor {

    /** This is the delay that we force when an action of the robot, which is supposed to last for a while, faile immediately.
     * This helps preventing dangerous light-speed loops.
     */
    private final static int ACTION_ABORT_DELAY_MILLIS = 1000 ;

    private MindbotTopicReceiver topicReceiver;
    private MindbotServiceRequester serviceReq;
    private NodeMainExecutor nodeMainExecutor;

    private static final String ACTION_MARKER = "$" ;

    // Construct executor
    public MindbotRobotExecutor(final PluginConfig config, final RunTimeProject project) { super(config, project); }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return ACTION_MARKER+id ;
    }


    private static int addressBytesToInt(byte[] addr_bytes) {
        int addr_int = ((addr_bytes[0] << 24) & 0xff000000) |
                ((addr_bytes[1] << 16) & 0x00ff0000) |
                ((addr_bytes[2] << 8) & 0x0000ff00) |
                ( addr_bytes[3] & 0x000000ff) ;
        return addr_int;
    }

    public static class NetBindingResult {

        public NetworkInterface intf ;
        public InetAddress addr ;

        public NetBindingResult(NetworkInterface interf, InetAddress address) {
            this.intf = interf ;
            this.addr = address ;
        }

    }

    /**
     *
     * @param remote_addr A (remote) IPv4 address to contact
     * @return The pair (interface,local_address), where the local address can be used to bind a Socket for a communication.
     * @throws SocketException At the monet, can be raised only if the list of interfaces can not be retrieved.
     */
    private static NetBindingResult getBestInterfaceFor(Inet4Address remote_addr) throws SocketException {
        byte[] remote_addr_bytes = remote_addr.getAddress();
        int remote_addr_int = addressBytesToInt(remote_addr_bytes);

        // Look for a compatible address on all the network interfaces...
        Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
        while(interfaces.hasMoreElements()) {
            NetworkInterface intf = interfaces.nextElement();

            // ... and through all of its addresses
            List<InterfaceAddress> intf_addrs = intf.getInterfaceAddresses();
            for(InterfaceAddress intf_addr : intf_addrs) {
                InetAddress local_addr = intf_addr.getAddress();
                byte[] local_addr_bytes = local_addr.getAddress();

                if(local_addr_bytes.length != remote_addr_bytes.length) {
                    // Likely, IP4 vs. IP6
                    continue;
                }

                int local_addr_int = addressBytesToInt(local_addr_bytes) ;

                // Compare the integer (4 bytes) version of the masked addresses.
                short intf_prefix = intf_addr.getNetworkPrefixLength() ;
                int net_mask = 0xffffffff << (32 - intf_prefix) ;
                int intf_addr_masked = local_addr_int & net_mask ;
                int remote_addr_masked = remote_addr_int & net_mask ;

                // If the two netwrok prefixes match, then we use this address
                if(intf_addr_masked == remote_addr_masked) {
                    return new NetBindingResult(intf, local_addr) ;
                }

            }
        }

        return null ;
    }

    @Override
    public final void launch() {

        //
        // Get the properties
        String ros_uri_prop = mConfig.getProperty("rosuri") ;
        if(ros_uri_prop == null) {
            String msg = "Missing property 'rosuri'" ;
            mLogger.failure(msg);
            throw new RuntimeException(msg) ;
        }
        URI ros_uri = URI.create(ros_uri_prop) ;
        mLogger.message("ROS URI is " + ros_uri);

        //
        // Retrieve the local IP which can be used to contact ROS
        NetBindingResult binding_result = null ;

        try {
            String ros_host = ros_uri.getHost() ;
            InetAddress ros_addr = Inet4Address.getByName(ros_host) ;
            binding_result = getBestInterfaceFor((Inet4Address)ros_addr);
        } catch (Exception e) {
            String msg = "Exception while searching for a local address for connection to ROS host " + ros_uri.getHost() + ": " + e.getMessage() ;
            mLogger.failure(msg);
            throw new RuntimeException(msg) ;
        }

        if(binding_result == null) {
            String msg = "Couldn't not find a suitable local address for binding with host " + ros_uri.getHost() ;
            mLogger.failure(msg);
            throw new RuntimeException(msg) ;
        }

        assert binding_result != null;

        String this_host = binding_result.addr.getHostAddress();
        mLogger.message("Using local address " + this_host + " on network interface '" + binding_result.intf.getDisplayName() + "' for ROS binding.");

        // Init the ROS main executor
        nodeMainExecutor = DefaultNodeMainExecutor.newDefault();

        //
        // Setup the topic Subscriber
        mLogger.message("Registering " + this_host + " as ROS Topic Receiver to master " + ros_uri);
        NodeConfiguration topicReceiverConfig = NodeConfiguration.newPublic(this_host, ros_uri);
        topicReceiver = new MindbotTopicReceiver();
        topicReceiverConfig.setNodeName("MindbotTopicSubscriber");
        nodeMainExecutor.execute(topicReceiver, topicReceiverConfig);

        //
        // Setup the service invocation node
        mLogger.message("Registering " + this_host + " as ROS Service Requester to master "  + ros_uri);
        NodeConfiguration serviceReqConfig = NodeConfiguration.newPublic(this_host, ros_uri);
        serviceReq = new MindbotServiceRequester(mLogger);
        serviceReqConfig.setNodeName("MindbotServiceRequester");
        nodeMainExecutor.execute(serviceReq, serviceReqConfig);

        mLogger.message("Waiting for setup of the Service Executor...");
        int i=0;
        while (! serviceReq.isSetupDone()) {
            mLogger.message("Waiting service request node setup (" + i + ")...");
            if(i>10) {
                break;
            }

            try { Thread.sleep(1000); } catch (Exception e) {}
            i++ ;
        }

        if(! serviceReq.isSetupDone()) {
            String msg = "ROS Node 'MindbotServiceRequester' didn't setup properly (setUpClient failed). Likely a connection timeout. Check ROS master.";
            mLogger.failure(msg);
            throw new RuntimeException(msg) ;
        }

        //
        // Setup the listener to catch incoming topic values and set them as project global variables
        mLogger.message("Setting Listener...");
        topicReceiver.setListener(new MindbotTopicReceiver.TopicListener() {
            @Override
            public void tcpChanged(Pose new_pose) {
                //mLogger.message("pose updated " + new_pose.getPosition());
                Point pos = new_pose.getPosition();
                if(mProject.hasVariable("robot_x")) { mProject.setVariable("robot_x", (float)pos.getX()) ; }
                if(mProject.hasVariable("robot_y")) { mProject.setVariable("robot_y", (float)pos.getY()) ; }
                if(mProject.hasVariable("robot_z")) { mProject.setVariable("robot_z", (float)pos.getZ()) ; }

                Quaternion or = new_pose.getOrientation();
                if(mProject.hasVariable("robot_or_w")) { mProject.setVariable("robot_or_w", (float)or.getW()) ; }
                if(mProject.hasVariable("robot_or_x")) { mProject.setVariable("robot_or_x", (float)or.getX()) ; }
                if(mProject.hasVariable("robot_or_y")) { mProject.setVariable("robot_or_y", (float)or.getY()) ; }
                if(mProject.hasVariable("robot_or_z")) { mProject.setVariable("robot_or_z", (float)or.getZ()) ; }
            }


            @Override
            public void ctrlStateChanged(String new_state) {
                if(mProject.hasVariable("robot_ctrl_state")) {
                    mProject.setVariable("robot_ctrl_state", new_state) ;
                }

            }

            @Override
            public void ctrlModeChanged(String new_mode) {
                if(mProject.hasVariable("robot_ctrl_mode")) {
                    mProject.setVariable("robot_ctrl_mode", new_mode) ;
                }

            }

            @Override
            public void error(String message) {
                mLogger.failure("Topic Listener reported an error: "+message);
            }
        });

        //
        // Done.
        mLogger.message("MindBotExecutor launched.");
    }

    @Override
    public final void unload() {

        if(nodeMainExecutor!=null) {
            nodeMainExecutor.shutdownNodeMain(topicReceiver);
            nodeMainExecutor.shutdownNodeMain(serviceReq);

            nodeMainExecutor.shutdown();
            nodeMainExecutor = null;
        }


    }

    private static HashMap<String, String> featureListToMap(LinkedList<ActionFeature> features) {
        HashMap<String, String> out = new HashMap<String, String>() ;
        for (ActionFeature f : features) {
            out.put(f.getKey(), f.getValNoQuotes()) ;
        }
        return out;
    }

    @Override
    public void execute(final AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            //
            // SPEECH activity
            SpeechActivity sa = (SpeechActivity) activity;
            String text_only = sa.getTextOnly(ACTION_MARKER).trim();

            if (text_only.isEmpty()) {
                LinkedList<String> timemarks = sa.getTimeMarks(ACTION_MARKER);
                for (String tm : timemarks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }

            } else {
                mLogger.warning("Sorry, the MindBot robot can NOT speak. Please, provde only actions and no text.");
            }

        } else {
            //
            // It is an [ACTION (with features)]
            // aka [COMMAND (with parameters)]

            String cmd = activity.getName() ;
            final LinkedList<ActionFeature> features_list = activity.getFeatures();
            final HashMap<String, String> features_map = featureListToMap(features_list) ;

            int actionID = -1 ;

            switch (cmd) {
                case "set_joint_target": {
                    // All parameters are in fact comma-separated lists of values.
                    // E.g.: botty: [set_joint_target joint_names="j1, j2, j3" positions="34.1, 5.4, 6.7" velocities="...", efforts="..." ].

                    String[] joint_names = features_map.get("joint_names").split(",");
                    List<String> joint_names_list = Arrays.stream(joint_names).map(String::trim).collect(Collectors.toList());
                    String[] positions_str = features_map.get("positions").split(",");
                    double[] positions_degs = Arrays.stream(positions_str).mapToDouble(p -> Double.parseDouble(p.trim())).toArray();
                    double[] positions_rads = Arrays.stream(positions_degs).map(Math::toRadians).toArray() ;
                    String[] velocities_str = features_map.get("velocities").split(",");
                    double[] velocities_degs = Arrays.stream(velocities_str).mapToDouble(p -> Double.parseDouble(p.trim())).toArray();
                    double[] velocities_rads = Arrays.stream(velocities_degs).map(Math::toRadians).toArray() ;
                    String[] efforts_str = features_map.get("efforts").split(",");
                    double[] efforts = Arrays.stream(efforts_str).mapToDouble(p -> Double.parseDouble(p.trim())).toArray();

                    actionID = serviceReq.setJointTarget(joint_names_list, positions_rads, velocities_rads, efforts);

                    break;
                }
                case "set_tcp_target": {

                    float x = Float.parseFloat(features_map.get("x"));
                    float y = Float.parseFloat(features_map.get("y"));
                    float z = Float.parseFloat(features_map.get("z"));
                    float or_w = Float.parseFloat(features_map.get("or_w"));
                    float or_x = Float.parseFloat(features_map.get("or_x"));
                    float or_y = Float.parseFloat(features_map.get("or_y"));
                    float or_z = Float.parseFloat(features_map.get("or_z"));

                    actionID = serviceReq.setTcpTarget(x, y, z, or_w, or_x, or_y, or_z);

                    break;
                }
                case "set_max_tcp_velocity": {

                    float x = Float.parseFloat(features_map.get("x"));
                    float y = Float.parseFloat(features_map.get("y"));
                    float z = Float.parseFloat(features_map.get("z"));
                    serviceReq.setMaxTcpVelocity(x, y, z);

                    break;
                }
                case "set_max_tcp_acceleration": {

                    float x = Float.parseFloat(features_map.get("x"));
                    float y = Float.parseFloat(features_map.get("y"));
                    float z = Float.parseFloat(features_map.get("z"));
                    serviceReq.setMaxTcpAcceleration(x, y, z);

                    break;
                }
                case "set_ctrl_state":

                    byte s = Byte.parseByte(features_map.get("state"));
                    serviceReq.setCtrlState(s);

                    break;
                case "set_ctrl_mode":

                    byte m = Byte.parseByte(features_map.get("mode"));
                    serviceReq.setCtrlMode(m);

                    break;
                case "set_min_clearance":

                    float min_clearance = Float.parseFloat(features_map.get("min_clearance"));
                    serviceReq.setMinClearanceService(min_clearance);

                    break;
                case "set_gripper_closure":

                    int closure = Integer.parseInt(features_map.get("closure")) ;
                    int velocity = Integer.parseInt(features_map.get("velocity")) ;
                    int force = Integer.parseInt(features_map.get("force")) ;
                    serviceReq.setGripperAction(closure, velocity, force);

                    break;
                default:
                    mLogger.failure("Unrecognized action '" + cmd + "'");
                    break;
            }

            if(actionID != -1) {
                MindbotServiceRequester.CallState result = serviceReq.waitAction(actionID) ;
                if(result == MindbotServiceRequester.CallState.FAILURE
                        || result== MindbotServiceRequester.CallState.ABORTED) {
                    // TODO -- This is a candidate information to be notified in the interface (pop up?).
                    mLogger.failure("Action '" + cmd + "' (localID=" + actionID + ") reported error: " + result + ". Delaying (" + ACTION_ABORT_DELAY_MILLIS + " millis)...");
                    try {
                        Thread.sleep(ACTION_ABORT_DELAY_MILLIS);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

        }
    }
}
