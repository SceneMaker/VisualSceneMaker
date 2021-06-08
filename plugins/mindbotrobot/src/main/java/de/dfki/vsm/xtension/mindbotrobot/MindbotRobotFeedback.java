package de.dfki.vsm.xtension.mindbotrobot;

/** A small interface specifying the methods available to the ROS wrappers
 *  to communicate back to VSM the status of the calls.
 */
public interface MindbotRobotFeedback {

    void setActionState(String res) ;

    void setActionMessage(String msg) ;

    void logWarning(String msg) ;

}
