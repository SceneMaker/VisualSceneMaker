# Mindbot Cobot communication plugin - development notes

This document explains the development guidelines for this plugin.

It contains also a small stand-alone class to test the communication without running a VSM project.


## Problem description

From a machine running VSM (VSM-machine, likely running windows), we want to connect to a remote machine (normally running Linux Ubuntu) on which a [ROS](https://www.ros.org/) instance runs (ROS-machine).

ROS is a complex system, not easy to install, which cannot be installed on the VSM-machine.

The goal is no write a VSM plugin which is able to communicate with the ROS-machine, without the need to install ROS also on the VSM-machine.

## Method

We use the rosjava bindings: <http://wiki.ros.org/rosjava>, and follow the documentation available only for ROS _kinetic_.

From the _Installation_ tutorial (<http://wiki.ros.org/rosjava/Tutorials/kinetic/Installation>), we follow the _No ROS installation_ option (<http://wiki.ros.org/rosjava/Tutorials/kinetic/No%20Ros%20Installation>).
This consists of using a Maven repository (<https://github.com/rosjava/rosjava_mvn_repo/raw/master>) to download pre-built jars for ROS 0.3.6.

### Complications

For the project Mindbot, the VSM plugin must communicate with the ROS-machine through custom messages and services which are not available in the default pre-compiled ROS jars.
For this reason, it is necessary to install rosjava on the ROS-machine and compile a java project able to convert the `mindbot_msgs` custom packages into java classes.
The create package is then manually copied as `lib/mindbot_msgs-0.0.0.jar`.

Instructions on how to build such package are given later.


## Building mindbot_msgs-x.y.z.jar

This is an operation to be performed on the ROS-machine.

### Install rosjava

Installing rosjava in the ROS-machine from the sources.
The official tutorial (<http://wiki.ros.org/rosjava/Tutorials/kinetic/Source%20Installation>) is a bit outdated. So we report fixes here:

Prerequisites:

```
sudo apt install ros-melodic-catkin ros-melodic-rospack python-wstool openjdk-8-jdk
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
```

Start installing rosjava in your home folder.

```
mkdir -p ~/rosjava/src
wstool init -j4 ~/rosjava/src https://raw.githubusercontent.com/rosjava/rosjava/kinetic/rosjava.rosinstall
source /opt/ros/melodic/setup.bash
cd ~/rosjava
# Make sure we've got all rosdeps and msg packages.
rosdep update
```

Fix missing packages (see <https://github.com/rosjava/genjava/issues/19#issuecomment-591173183>):

Run:

    nano -w src/rosjava_messages/CMakeLists.txt

and remove line:

    world_canvas_msgs

Run:

    nano -w src/rosjava_messages/package.xml
    
and remove line

    <build_depend>world_canvas_msgs</build_depend>

Now continue...

```
rosdep install --from-paths src -i -y
catkin_make
```

Now rojava is installed.

### Retrieve the mindbot_msgs definition

Get the `mindbot_msgs` definitions from the mindbot repository:

```
git clone https://mindbotgit.cloud.garrservices.it/wp5/mindbot-robot-control.git
```

Now directory `mindbot_robot_control/mindbot_stack/` contains folder `mindbot_msgs/`


### Setup the rosjava workspace

Activate the ROS environment:

```
source /opt/ros/melodic/setup.bash
source ~/rosjava/devel/setup.bash
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
```

Create a folder for the rosjava workspace, name it how you like, for this tutorial we name it 'mindbot_vsm_ws'. 
With the following commands, we create a source folder, and rosjava creates for us a package and a project which contains a Subscriber and a Publisher:

```
mkdir mindbot_vsm_ws
cd mindbot_vsm_ws
mkdir -p src
cd src
catkin_create_rosjava_pkg communication_package
cd ..
catkin_make
```

`catkin_make` builds the project and creates a `build` and a `devel` folder.
Now activate the new devel environment and create the project:

```
source devel/setup.bash
cd src/communication_package
catkin_create_rosjava_project communication_project
cd ../..
catkin_make
```

We have created a project which contains a subscriber (`Listener.java`) and a publisher (`Talker.java`). Check it with:

    ls -la src/communication_package/communication_project/src/main/java/com/github/communication_package/communication_project/

but we will not use them.

Now we need to move the `mindbot_msgs` into the top `src` folder, where also the folder `communication_package` is located.

```
cd path/to/mindbot_vsm_ws/
cp -r <path/to>/mindbot_ws/src/mindbot-robot-control/mindbot_stack/mindbot_msgs/ src/
```


Now you need to change the following file:

    nano -w src/communication_package/CMakeLists.txt

and update the `find_package()` and `catkin_package()` directoves.
Replace the `# Catkin` section:

```
##############################################################################
# Catkin
##############################################################################

find_package(catkin REQUIRED
rosjava_build_tools
message_generation
mindbot_msgs)


# Set the gradle targets you want catkin's make to run by default, e.g.
#   catkin_rosjava_setup(installDist)
# Note that the catkin_create_rosjava_xxx scripts will usually automatically
# add tasks to this for you when you create subprojects.
catkin_rosjava_setup(installDist publish)

catkin_package(CATKIN_DEPENDS
message_runtime
mindbot_msgs)
```

And this file:

    nano -w src/communication_package/package.xml

and extend the dependencies:

```
<!-- AFTER these 2 lines -->
<buildtool_depend>catkin</buildtool_depend>
<build_depend>rosjava_build_tools</build_depend>
<!-- ADD THESE 4 lines-->
<build_depend>message_generation</build_depend>
<build_depend>mindbot_msgs</build_depend>
<exec_depend>mindbot_msgs</exec_depend>
<exec_depend>message_runtime</exec_depend>
```

and finally:

    nano -w src/communication_package/communication_project/build.gradle

and extend the `dependencies` section:

```
compile 'org.ros.rosjava_core:rosjava:[0.3,0.4)'
compile 'org.ros.rosjava_messages:mindbot_msgs:[0.0, 0.1)'
```

Once again in a console, be sure you are in folder `mindbot_vsm_ws` and:

```
catkin_make  # This will give an error
catkin_make
```

Yes, run it two times. The first will not find the mindbot_msgs. The second will get it done.

You can find the jar in `mindbot_vsm_ws/build/mindbot_msgs/java/mindbot_msgs/build/libs/mindbot_msgs-0.0.0.jar`.

    ls -la build/mindbot_msgs/java/mindbot_msgs/build/libs/

From now on, every time you need to update the jar, you just need to initialize the environment (_source_s and _JAVA_HOME_) and run `catkin_make` from here.

```
cd path/to/mindbot_vsm_ws/
cp -r <path/to>/mindbot_ws/src/mindbot_robot_control/mindbot_stack/mindbot_msgs/ src/
source /opt/ros/melodic/setup.bash
source ~/rosjava/devel/setup.bash
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
source devel/setup.bash
catkin_make clean
catkin_make
catkin_make
```
