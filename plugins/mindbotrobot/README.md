# MindBot Robot Controller

This plugin (`mindbotrobot`) is developed within project MindBot (<https://www.mindbot.eu>) and serves to pilot the Collaborative robots using Visual Scene Maker.

The plugin uses rosjava (<http://wiki.ros.org/rosjava>) to connect to a remote Linux machine running the robot within a ROS (<https://www.ros.org/>) architecture.
 
## Project setup

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

# How to Run the ROSJava Code to connect with the Mindbot

* Go inside the folder 'MindbotCommunication'
* Run the following commands: (you can also add the first two commands in your 'gedit ~/.bashrc' at the end of the file, then you do not need to run these everytime you edit your Publisher/Subscriber)

  ```
  source /opt/ros/melodic/setup.bash
  source ~/rosjava/devel/setup.bash
  catkin_make  (This creates the devel folder)
  source devel/setup.bash 
  cd src/communication_package/communication_project/
  ../gradlew install
  ```



* Now you have set up everything you need to either run a Publisher or a Listener
* To run the Publisher, run the command:
  
    rosrun communication_package communication_project communication_package.communication_project.MindbotPublisher
    
* To run the Subscriber, run the command:
    
    rosrun communication_package communication_project communication_package.communication_project.MindbotSubscriber



###Instructions to launch the Mindbot:
* Go inside the folder where your mindbot workspace is locate
* Run the following commands:
  ```
    catkin_build
    source devel/setup.bash
    roslaunch mindbot mindbot.launch
  ```