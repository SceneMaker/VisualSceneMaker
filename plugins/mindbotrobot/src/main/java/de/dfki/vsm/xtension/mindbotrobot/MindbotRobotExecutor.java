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

public final class MindbotRobotExecutor extends ActivityExecutor implements MindbotRobotFeedback {

    /** This is the delay that we force when an action of the robot, which is supposed to last for a while, fails immediately.
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
        NetTools.NetBindingResult binding_result = null ;

        try {
            String ros_host = ros_uri.getHost() ;
            InetAddress ros_addr = Inet4Address.getByName(ros_host) ;
            binding_result = NetTools.getBestInterfaceFor((Inet4Address)ros_addr);
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
        serviceReq = new MindbotServiceRequester(this);
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
                if(mProject.hasVariable("robot_tcp_x")) { mProject.setVariable("robot_tcp_x", (float)pos.getX()) ; }
                if(mProject.hasVariable("robot_tcp_y")) { mProject.setVariable("robot_tcp_y", (float)pos.getY()) ; }
                if(mProject.hasVariable("robot_tcp_z")) { mProject.setVariable("robot_tcp_z", (float)pos.getZ()) ; }

                Quaternion or = new_pose.getOrientation();
                if(mProject.hasVariable("robot_tcp_or_w")) { mProject.setVariable("robot_tcp_or_w", (float)or.getW()) ; }
                if(mProject.hasVariable("robot_tcp_or_x")) { mProject.setVariable("robot_tcp_or_x", (float)or.getX()) ; }
                if(mProject.hasVariable("robot_tcp_or_y")) { mProject.setVariable("robot_tcp_or_y", (float)or.getY()) ; }
                if(mProject.hasVariable("robot_tcp_or_z")) { mProject.setVariable("robot_tcp_or_z", (float)or.getZ()) ; }
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
                case "set_joint_target":
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

                case "set_tcp_target":

                    float x = Float.parseFloat(features_map.get("x"));
                    float y = Float.parseFloat(features_map.get("y"));
                    float z = Float.parseFloat(features_map.get("z"));
                    float or_w = Float.parseFloat(features_map.get("or_w"));
                    float or_x = Float.parseFloat(features_map.get("or_x"));
                    float or_y = Float.parseFloat(features_map.get("or_y"));
                    float or_z = Float.parseFloat(features_map.get("or_z"));

                    actionID = serviceReq.setTcpTarget(x, y, z, or_w, or_x, or_y, or_z);

                    break;

                case "set_max_tcp_velocity":

                    float tcp_v_x = Float.parseFloat(features_map.get("x"));
                    float tcp_v_y = Float.parseFloat(features_map.get("y"));
                    float tcp_v_z = Float.parseFloat(features_map.get("z"));
                    serviceReq.setMaxTcpVelocity(tcp_v_x, tcp_v_y, tcp_v_z);

                    break;

                case "set_max_tcp_acceleration":

                    float tcp_acc_x = Float.parseFloat(features_map.get("x"));
                    float tcp_acc_y = Float.parseFloat(features_map.get("y"));
                    float tcp_acc_z = Float.parseFloat(features_map.get("z"));
                    serviceReq.setMaxTcpAcceleration(tcp_acc_x, tcp_acc_y, tcp_acc_z);

                    break;

                case "set_ctrl_state":

                    byte ctrl_state = Byte.parseByte(features_map.get("state"));
                    serviceReq.setCtrlState(ctrl_state);

                    break;

                case "set_ctrl_mode":

                    byte ctrl_mode = Byte.parseByte(features_map.get("mode"));
                    serviceReq.setCtrlMode(ctrl_mode);

                    break;

                case "set_min_clearance":
                    float min_clearance = Float.parseFloat(features_map.get("min_clearance"));
                    serviceReq.setMinClearanceService(min_clearance);

                    break;

                case "set_gripper_closure":

                    int grip_closure = Integer.parseInt(features_map.get("closure")) ;
                    int grip_velocity = Integer.parseInt(features_map.get("velocity")) ;
                    int grip_force = Integer.parseInt(features_map.get("force")) ;
                    serviceReq.setGripperAction(grip_closure, grip_velocity, grip_force);

                    break;

                case "detect_object":
                    String detection_obj_name = features_map.get("name") ;
                    serviceReq.setVisualDetection(detection_obj_name);

                    break;

                default:
                    mLogger.failure("Unrecognized action '" + cmd + "'");
            }

            if(actionID != -1) {
                MindbotServiceRequester.CallState result = serviceReq.waitAction(actionID) ;
                if(result == MindbotServiceRequester.CallState.UNREACHABLE
                        || result== MindbotServiceRequester.CallState.FAILED) {
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


    private final String ROBOT_ACTION_VAR = "robot_action_state" ;
    private final String ROBOT_MESSAGE_VAR = "robot_action_message" ;

    @Override
    public void setActionState(String res) {
        if(mProject.hasVariable(ROBOT_ACTION_VAR)) {
            mProject.setVariable(ROBOT_ACTION_VAR, res);
        }
    }

    @Override
    public void setActionMessage(String msg) {
        if(mProject.hasVariable(ROBOT_MESSAGE_VAR)) {
            mProject.setVariable(ROBOT_MESSAGE_VAR, msg);
        }
    }

    @Override
    public void logWarning(String msg) {
        mLogger.warning(msg);
    }
}
