# MindBot Robot Controller

This plugin (`mindbotrobot`) is developed within project MindBot (<https://www.mindbot.eu>) and allows to pilot the Collaborative robots through Visual Scene Maker.

The plugin uses rosjava (<http://wiki.ros.org/rosjava>) to connect to a remote Linux machine running the robot within a ROS (<https://www.ros.org/>) architecture.

## Prerequisites

The plugin can be tested on a network where the MindBot robot is running. Please, refer to the MindBot code repository <https://mindbotgit.cloud.garrservices.it/matteolavitnicora/mindbot_robot_control.git>

Refer to the official VSM documentation on how to set up a project and execute scenes: <http://scenemaker.dfki.de/tutorial.html>.


## Plugin setup

* Open a new VSM project and add a `MindBotRobotExecutor` device.
* Setup the device properties:
  * `rosuri`: The URI (e.g., http://localhost:11311) on which the main ROS system and the robot are running.
* Add an agent for the device.

![MindBotRobot plugin configuration](images/VSM-MindBotRobotConfig.png)

* Setup the project variables:
  * `robot_ctrl_mode` (String): returns the mode of the robot among: `MODE0`, `MODE1`, `MODE2`, `Undefined`.
  * `robot_ctrl_state` (String): returns the state of the robot among: `ON`, `OFF`, `ERROR`, `Undefined`.
  * A set of variables for the TCP pose of the robot:
    * `robot_x`, `robot_y`, `robot_z` (Float) for the TCP position; and
    * `robot_or_w`,`robot_or_x`, `robot_or_y`, `robot_or_z` (Float) for the TCP orientation quaternion;

Those variables will be continuously updated when the project runs.

![Project Variables](images/VSM-MindBotControlDemo.png)


## Invoking cobot commands

The MindBot cobot can not speak, hence there is no support for text parsing.
Scene utterances must be composed solely by actions. E.g.:

    <agent_name>: [<action_name> <parameter1>="<value1>" <parameter2>="<value2>"].

_The full-stop at the end is mandatory!_

The following actions can be invoked within scenes:

* `set_joint_target` The parameters specify a comma-separated list of joint names and their respective target position (rotation), velocity, and effort.
  * Parameters:
    * `joint_names`
    * `positions`
    * `velocities`
    * `efforts`
  * Example: TODO
* `set_tcp_target` Set position and rotation of the robot end effector.
  * Parameters:
    * `x`
    * `y`
    * `z`
    * `rot_w`
    * `rot_x`
    * `rot_y`
    * `rot_z`
  * Example: `[set_tcp_target x=0.2 y=0.1 z=0.5 or_w=1 or_x=0 or_y=0 or_z=0]`
* `set_max_tcp_velocity`
  * Parameters:
    * `x`
    * `y`
    * `z`
  * Example: TODO
* `set_max_tcp_acceleration`
  * Parameters:
    * `x`
    * `y`
    * `z`
  * Example: TODO
* `set_ctrl_state`
  * Parameters:
    * `state` Either 0, 1, or 2.
  * Example: TODO
* `set_ctrl_mode`
  * Parameters:
    * `mode` Either 0, 1, or 2.
  * Example: TODO
* `set_min_clearance`
  * Parameters:
    * `min_clearance`
  * Example: TODO
