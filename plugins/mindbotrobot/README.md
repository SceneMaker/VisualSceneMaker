# MindBot Robot Controller

This plugin (`mindbotrobot`) is developed within project MindBot (<https://www.mindbot.eu>) and allows to pilot the Collaborative Robots (Cobots) through Visual Scene Maker.

The plugin uses rosjava (<http://wiki.ros.org/rosjava>) to connect to a remote Linux machine running the robot within a ROS (<https://www.ros.org/>) architecture.

## Prerequisites

The plugin can be tested on a network where the MindBot robot is running. Please, refer to the MindBot code repository <https://mindbotgit.cloud.garrservices.it/wp5/mindbot-robot-control>.

Refer to the official VSM documentation on how to set up a project and execute scenes: <http://scenemaker.dfki.de/tutorial.html>.


## Plugin setup

* Open a new VSM project and add a `MindBotRobotExecutor` device.
* Setup the device properties:
  * `rosuri` The URI (e.g., http://localhost:11311) on which the main ROS system and the robot are running.
* Add an agent for the device.

![MindBotRobot plugin configuration](images/VSM-MindBotRobotConfig.png)

* Setup the project global variables that will be updated while the project runs:
  * `robot_ctrl_mode` (String): returns the mode of the robot among: `MODE0`, `MODE1`, `MODE2`, `Undefined`.
  * `robot_ctrl_state` (String): returns the state of the robot among: `ON`, `OFF`, `ERROR`, `Undefined`.
  * A set of variables for the TCP pose of the robot:
    * `robot_tcp_x`, `robot_tcp_y`, `robot_tcp_z` (Float) for the TCP position; and
    * `robot_tcp_or_w`, `robot_tcp_or_x`, `robot_tcp_or_y`, `robot_tcp_or_z` (Float) for the TCP orientation quaternion;
  * A set of action state and message variables:
    * `robot_configuration_state` and `robot_configuration_message` (String)
    * `robot_arm_state` and `robot_arm_message` (String)
    * `robot_gripper_state` and `robot_gripper_message` (String)
    * `robot_detection_state` and `robot_detection_message` (String)
    * The values that can be assumed by these variables are reported in a later section.

![Project Variables](images/VSM-MindBotControlDemo.png)

Please, see the `ExampleProject` distributed with this plugin for a comprehensive example testing all of the above-mentioned commands.


## Invoking robot actions

The MindBot robot cannot speak, hence there is no support for text parsing.
Scene utterances must be composed solely by actions:

    <agent_name>: [<action_name> <parameter1>="<value1>" <parameter2>="<value2>"].
    
For example:

    botty: [set_tcp_target x=0.64 y=-0.15 z=0.95 or_w=0.50 or_x=-0.50 or_y=-0.50 or_z=0.50].

_The full-stop at the end is mandatory!_

The following actions can be invoked within scenes:

* `set_joint_target` (Blocking) The parameters specify a comma-separated list of joint names and their respective target position (rotation in degrees), velocity (in degrees/sec), and effort (N/m).
  * Category: `arm`
  * Parameters:
    * `joint_names` The comma-separated list of the join names. This has to be compatible with the name of the joints of the robot.
    * `positions` The comma-separated list of rotations of all joints, in degrees.
    * `velocities`
    * `efforts`
  * Example (names for iiwa robot): `[set_joint_target joint_names='iiwa_joint_0,iiwa_joint_1,iiwa_joint_2,iiwa_joint_3,iiwa_joint_4,iiwa_joint_5,iiwa_joint_6' positions='10,10,10,10,10,10,10' velocities='1,1,2,2,3,3,5' efforts='10,10,30,30,50,50,80']`.
    It sets all the joints at 10 degrees rotation.
* `set_tcp_target` (Blocking) Set position and rotation (orientation) of the robot end effector.
  * Category: `arm`
  * Parameters:
    * `x`
    * `y`
    * `z`
    * `or_w`
    * `or_x`
    * `or_y`
    * `or_z`
  * Example: `[set_tcp_target x=0.2 y=0.1 z=0.5 or_w=1 or_x=0 or_y=0 or_z=0].`
* `set_max_tcp_velocity` Set the velocity limits in cartesian coordinates.
  * Category: `configuration`
  * Parameters:
    * `x`
    * `y`
    * `z`
  * Example: `[set_max_tcp_velocity x=0.5 y=1 z=1].`
* `set_max_tcp_acceleration` Set the acceleration limits in cartesian coordinates.
  * Category: `configuration`
  * Parameters:
    * `x`
    * `y`
    * `z`
  * Example: `[set_max_tcp_acceleration x=0.5 y=1 z=1].`
* `set_ctrl_state` Set the control state (OFF=0, ON=1, ERROR=2).
  * Category: `configuration`
  * Parameters:
    * `state` Either 0, 1, or 2.
  * Example: `[set_ctrl_state state=0].`
* `set_ctrl_mode` Set the control mode (MODE0=0, MODE1=1, MODE2=2).
  * Category: `configuration`
  * Parameters:
    * `mode` Either 0, 1, or 2.
  * Example: `[set_ctrl_mode mode=0].`
* `set_min_clearance` Set the minimum accepted distance between robot and operator.
  * Category: `configuration`
  * Parameters:
    * `min_clearance`
  * Example: `[set_min_clearance min_clearance=0.5].`
* `set_gripper_closure` (Blocking) Set the closure of the gripper, or better, tries to reach the specified closure, at a given speed, and stops before if a certain resistance force threshold limit is reached.
  * Category: `gripper`
  * Parameters:
    * `closure` An integer between 0 and 255 setting the distance between the gripper tips. Indicative values (subject to changes among robots): 0=60mm, 255=10mm.
    * `velocity` An integer between 0 and 255 setting the movement speed of the gripper. Indicative values (subject to changes among robots): 0=20mm/sec, 255=150mm/sec.
    * `force` An integer between 0 and 255 setting the maximum force exerted by the gripper before stopping. Indicative values (subject to changes among robots): 0=no_force 255=5Kg.
  * Example: `[set_gripper_closure closure=255 velocity=150 force=2]`
* `detect_object` (Blocking) Starts the procedure for a visual detection of an object.
  * Category: `detection`
  * Parameters:
    * `name` The name of the object to detect. The name will be resolved and associated to some physical entity at the robot side.
  * Example: `[detect_object name='gear1']`
  * For this kind of message, if succeeded, the response will bring the Pose (translation and quaternion) of the detected object. This will b set in the VSM global variables:
    * `detected_pose_x`
    * `detected_pose_y`
    * `detected_pose_z`
    * `detected_pose_rw`
    * `detected_pose_rx`
    * `detected_pose_ry`
    * `detected_pose_rz`
    
    Attention: If the variables are not defined, no warnings will be raised. Hence, check the syntax carefully.

  
## Actions feedback

Actions can be _blocking_ or not.
If an action is NOT blocking, it's execution consists in invoking a remote ROS service (on the robot side) and immediately reporting the result of the remote call.
An example of non blocking action is `set_min_clearance`, which sets a parameter on the robot.

Differently, if an action is _blocking_, during its execution the VSM node will stop and be unlocked only after the `action_done` service is called back, upon execution termination.
An example is the `set_tcp_target` action, which unblock the execution of a node only when the TCP (the last joint of the robot arm, holding the gripper) of the robot reaches the requested position.

During their execution, actions will assume a sequence of states, which are 
updated according to the following scheme (Java code extract):

```java
    // Possible state paths for immediate actions, which do not need to wait for the action_done service invoked:
    // CALLED -> UNREACHABLE
    // CALLED -> UNSUCCESSFUL
    // CALLED -> DONE

    // Possible state paths for actions waiting for an answer (action_done) from the robot:
    // CALLED -> UNREACHABLE
    // CALLED -> UNSUCCESSFUL
    // CALLED -> EXECUTING -> FAILED
    // CALLED -> EXECUTING -> DONE

    public enum CallState {
        CALLED,     // The remote ROS service has been called
        UNREACHABLE,    // The remote ROS service answered FAILURE. The action will not be executed. Don't wait for it.
        UNSUCCESSFUL, // The remote ROS service was reachable but the recipient responded that the request can not be accomplished. Don't wait for it.
        EXECUTING,    // The remote ROS service answered SUCCESS. Action is in execution on the ROS side. Expect an action_done call when finished.
        FAILED,    // The remote ROS called back the action_done service to inform that the call couldn't execute properly.
        DONE        // The remote ROS called back the action_done to inform that the call was executed successfully.
}
```

Action states will be written in project global variables, as described in the next section.


## Action categories and global state variables

Actions are grouped into categories (or types):

```java
    enum ActionType {
        Configuration,  // Actions to set system parameters, whose execution is normally instantaneous
        Arm,            // Actions moving the main robot arm
        Gripper,        // Actions controlling the robot gripper
        Detection       // Actions performing the detection of objects (e.g., via a vision module)
    }
```

Each action type is associated to a pair of project variables (`robot_<type>_state` and `robot_<type>_message`), already listed above, such as:

  * `robot_configuration_state` and
  * `robot_configuration_message`

The execution of any action on the robot results in setting the `state` and `message` variables of the corresponding category as the execution advances.

The goal of this separation is to allow for the parallel execution of actions that might last a significant amount of time.
For example, the gripper of the robotic arm can open while the arm moves to a new position.
Similarly, the object detection routine can run while the robot arm explores an area of the working space.

Having the results is different variables allows for the configuration of scene flows branching the execution of different actions in different nodes,
checking the result of the execution on different non-overlapping variables, and then joining the flow if not error occurs.
