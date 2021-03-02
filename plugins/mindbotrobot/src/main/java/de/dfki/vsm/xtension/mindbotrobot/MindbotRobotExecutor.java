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
import org.ros.address.InetAddressFactory;
import org.ros.node.DefaultNodeMainExecutor;
import org.ros.node.NodeConfiguration;
import org.ros.node.NodeMainExecutor;

import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public final class MindbotRobotExecutor extends ActivityExecutor {

    /** This is the delay that we force when an action of the robot, which is supposed to last for a while, faile immediately.
     * This helps preventing dangerous light-speed loops.
     */
    private final static int ACTION_ABORT_DELAY_MILLIS = 500 ;

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

        //
        // The local IP/port host use to contact ROS
        String this_host = InetAddressFactory.newNonLoopback().getHostAddress();

        // Init the ROS main executor
        nodeMainExecutor = DefaultNodeMainExecutor.newDefault();

        //
        // Setup the topic Subscriber
        mLogger.message("Registering ROS Topic Receiver");
        NodeConfiguration topicReceiverConfig = NodeConfiguration.newPublic(this_host, ros_uri);
        topicReceiver = new MindbotTopicReceiver();
        topicReceiverConfig.setNodeName("MindbotTopicSubscriber");
        nodeMainExecutor.execute(topicReceiver, topicReceiverConfig);

        //
        // Setup the service invocation node
        mLogger.message("Registering ROS Service Requester");
        NodeConfiguration serviceReqConfig = NodeConfiguration.newPublic(this_host, ros_uri);
        serviceReq = new MindbotServiceRequester();
        serviceReqConfig.setNodeName("MindbotServiceRequester");
        nodeMainExecutor.execute(serviceReq, serviceReqConfig);

        mLogger.message("Waiting for setup of the Service Executor...");
        int i=0;
        while (! serviceReq.isSetupDone()) {
            mLogger.failure("Waiting service request node setup (" + i + ")...");
            if(i>10) {
                mLogger.failure("ROS Node Main Executor didn't terminate properly. Likely: connection timeout");
                break;
            }

            try { Thread.sleep(1000); } catch (Exception e) {}
            i++ ;
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
                    double[] positions = Arrays.stream(positions_str).mapToDouble(p -> Double.parseDouble(p.trim())).toArray();
                    String[] velocities_str = features_map.get("velocities").split(",");
                    double[] velocities = Arrays.stream(velocities_str).mapToDouble(p -> Double.parseDouble(p.trim())).toArray();
                    String[] efforts_str = features_map.get("efforts").split(",");
                    double[] efforts = Arrays.stream(efforts_str).mapToDouble(p -> Double.parseDouble(p.trim())).toArray();

                    actionID = serviceReq.setJointTarget(joint_names_list, positions, velocities, efforts);

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
                    serviceReq.setCtrlState(m);

                    break;
                case "set_min_clearance":

                    float min_clearance = Float.parseFloat(features_map.get("min_clearance"));
                    serviceReq.setMinClearanceService(min_clearance);

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
                    mLogger.failure("Action '" + cmd + "' (id=" + actionID + ") reported error: " + result + ". Delaying (" + ACTION_ABORT_DELAY_MILLIS + " millis)...");
                    try {
                        wait(ACTION_ABORT_DELAY_MILLIS);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

        }
    }
}
