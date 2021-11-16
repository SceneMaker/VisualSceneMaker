package de.dfki.vsm.xtension.mindbotrobot;

/** A small interface specifying the methods available to the ROS wrappers
 *  to communicate back to VSM the status of the calls.
 */
public interface MindbotRobotFeedback {

    void setActionState(int action_id, String res) ;

    void setActionMessage(int action_id, String msg) ;

    void logWarning(int action_id, String msg) ;

    void setDetectedPose(int action_id, geometry_msgs.Pose p);
}
