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
import java.util.LinkedList;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public final class MindbotRobotExecutor extends ActivityExecutor {


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
            mLogger.failure("Waiting service request node setup (" +i+")...");
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
                mLogger.message("pose updated " + new_pose.getPosition());
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

    @Override
    public void execute(final AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            //
            // SPEECH activity
            SpeechActivity sa = (SpeechActivity) activity;

            String text_only = sa.getTextOnly(ACTION_MARKER).trim();
            LinkedList<String> time_marks = sa.getTimeMarks(ACTION_MARKER);

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
            final LinkedList<ActionFeature> features = activity.getFeatures();

            if(cmd.equals("set_joint_target")) {

            } else if(cmd.equals("set_tcp_target")) {

                float x = Float.parseFloat(features.get(0).getVal()) ;
                float y = Float.parseFloat(features.get(1).getVal()) ;
                float z = Float.parseFloat(features.get(2).getVal()) ;
                float or_w = Float.parseFloat(features.get(3).getVal()) ;
                float or_x = Float.parseFloat(features.get(4).getVal()) ;
                float or_y = Float.parseFloat(features.get(5).getVal()) ;
                float or_z = Float.parseFloat(features.get(6).getVal()) ;
                serviceReq.setTcpTarget(x, y, z, or_w, or_x, or_y, or_z);

            } else if(cmd.equals("set_max_tcp_velocity")) {
            } else if(cmd.equals("set_max_tcp_acceleration")) {
            } else if(cmd.equals("set_ctrl_state")) {
            } else if(cmd.equals("set_ctrl_mode")) {
            } else if(cmd.equals("set_min_clearance")) {
                mLogger.failure("Action 'set_min_clearance' not supported, yet");
            } else {
                mLogger.failure("Unrecognized command '" + cmd + "'");
            }

        }
    }
}
