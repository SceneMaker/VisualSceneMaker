package de.dfki.vsm.xtension.mindbotrobot;

import geometry_msgs.Point;
import geometry_msgs.Pose;
import geometry_msgs.Quaternion;
import org.ros.message.MessageListener;
import org.ros.namespace.GraphName;
import org.ros.node.*;
import org.ros.node.topic.Subscriber;

/**
 * A {@link Subscriber} {@link NodeMain} for Mindbot.
 *
 * @author sarah.hoffmann@dfki.de (Sarah Hoffmann)
 */

public class MindbotTopicReceiver implements NodeMain {

    protected Subscriber<geometry_msgs.PoseStamped> subscriberTcpState;
    protected Subscriber<mindbot_msgs.CtrlState> subscriberCtrlState;
    protected Subscriber<mindbot_msgs.CtrlMode> subscriberCtrlMode;
    public static boolean finished;


    public static String ctrlStateToString (byte state_code) {
        if(state_code == 0) {
            return "OFF";
        } else if (state_code == 1) {
            return "ON" ;
        } else if (state_code == 2) {
            return "ERROR" ;
        } else {
            return "Undefined" ;
        }
    }

    public static String ctrlModeToString(byte mode_code) {
        if(mode_code == 0) {
            return "MODE0";
        } else if (mode_code == 1) {
            return "MODE1" ;
        } else if (mode_code == 2) {
            return "MODE2" ;
        } else {
            return "Undefined" ;
        }
    }

    /** Interface for topic listeners. */
    public interface TopicListener {
        void tcpChanged(Pose new_pose) ;
        void ctrlStateChanged(String new_state) ;
        void ctrlModeChanged(String new_mode) ;
        void error(String message) ;
    }


    /** The listener forwarding the information about changed tcp, state, and mode. */
    private TopicListener _listener = null ;

    public TopicListener getListener() {
        return _listener;
    }

    public void setListener(TopicListener listener) {
        this._listener = listener;
    }

    /** Caches the value of the last received Pose */
    private Pose _lastPose = null ;
    /** Caches the value of the last received State */
    private String _lastState = null ;
    /** Caches the value of the last received Mode */
    private String _lastMode = null ;


    @Override
    public GraphName getDefaultNodeName() {
        return GraphName.of("mindbot/vsm/RobotTopicReceiver");
    }

    @Override
    public void onStart(ConnectedNode connectedNode) {
        setupSubscribers(connectedNode);
        setupListeners();
    }

    @Override
    public void onShutdown(Node node) {
        if(subscriberCtrlMode != null) {
            subscriberCtrlMode.removeAllMessageListeners();
            subscriberCtrlMode.shutdown();
            subscriberCtrlMode = null;
        }

        if(subscriberCtrlState != null) {
            subscriberCtrlState.removeAllMessageListeners();
            subscriberCtrlState.shutdown();
            subscriberCtrlState = null;
        }

        if(subscriberTcpState != null) {
            subscriberTcpState.removeAllMessageListeners();
            subscriberTcpState.shutdown();
            subscriberTcpState = null;
        }

        node.removeListeners();
        node.shutdown();
    }

    @Override
    public void onShutdownComplete(Node node) {

    }

    @Override
    public void onError(Node node, Throwable throwable) {
        if(_listener != null) {
            _listener.error(throwable.getMessage());
        }

    }

    private void setupSubscribers(ConnectedNode connectedNode) {
        subscriberTcpState = connectedNode.newSubscriber("/iiwa/tcp_state", geometry_msgs.PoseStamped._TYPE);
        subscriberCtrlState = connectedNode.newSubscriber("/iiwa/ctrl_state", mindbot_msgs.CtrlState._TYPE);
        subscriberCtrlMode = connectedNode.newSubscriber("/iiwa/ctrl_mode", mindbot_msgs.CtrlMode._TYPE);
    }

    private static boolean _equalPoses(Pose pose1, Pose pose2) {
        Point p1 = pose1.getPosition();
        Point p2 = pose2.getPosition() ;
        Quaternion or1 = pose1.getOrientation();
        Quaternion or2 = pose2.getOrientation() ;
        return p1.getX() == p2.getX() &&
                p1.getY() == p2.getY() &&
                p1.getZ() == p2.getZ() &&
                or1.getW() == or2.getW() &&
                or1.getX() == or2.getX() &&
                or1.getY() == or2.getY() &&
                or1.getZ() == or2.getZ() ;



    }

    private void setupListeners() {

        subscriberTcpState.addMessageListener(new MessageListener<geometry_msgs.PoseStamped>() {
            @Override
            public void onNewMessage(geometry_msgs.PoseStamped message) {
                if(_listener == null)
                    return ;

                Pose pose = message.getPose();

                if ((_lastPose == null) || ! _equalPoses(_lastPose, pose)) {
                    _listener.tcpChanged(pose);
                    _lastPose = pose ;
                }

            }
        });

        subscriberCtrlState.addMessageListener(new MessageListener<mindbot_msgs.CtrlState>() {
            public void onNewMessage(mindbot_msgs.CtrlState message) {
                if(_listener == null)
                    return ;

                byte ctrl_state = message.getCtrlState() ;
                String ctrl_state_str = ctrlStateToString(ctrl_state) ;

                if (_lastState == null || !_lastState.equals(ctrl_state_str)) {
                    _listener.ctrlStateChanged(ctrl_state_str);
                    _lastState = ctrl_state_str ;
                }

            }
        });

        subscriberCtrlMode.addMessageListener(new MessageListener<mindbot_msgs.CtrlMode>() {
            public void onNewMessage(mindbot_msgs.CtrlMode message) {
                if(_listener == null)
                    return ;

                byte ctrl_mode = message.getCtrlMode();
                String ctrl_mode_str = ctrlModeToString(ctrl_mode) ;

                if(_lastMode == null || !_lastMode.equals(ctrl_mode_str)) {
                    _listener.ctrlModeChanged(ctrl_mode_str);
                    _lastMode = ctrl_mode_str ;
                }
            }
        });

    }
}
