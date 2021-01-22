# Test ROS Communication

This is a standalone Java project to test the communication with a ROS machine as a standalone executable.

This is NOT a VSM plugin! This is just a small test project. Code here for testing and sharing.

## Problem

From a machine running VSM (VSM-machine, likely running windows), we want to connect to a remote machine (normally running Linux Ubuntu) on which a [ROS](https://www.ros.org/) instance runs (ROS-machine).

ROS is a complex system, not easy to install, which can not be installed on the VSM-machine.

The goal is no write a VSM plugin which is able to communicate with the ROS-machine, without the need to install ROS also on the VSM-machine.

## Method

We use the rosjava bindings: <http://wiki.ros.org/rosjava>, and follow the documentation available only for ROS _kinetic_.

From the _Installation_ tutorial (<http://wiki.ros.org/rosjava/Tutorials/kinetic/Installation>), we follow the _No ROS installation_ option (<http://wiki.ros.org/rosjava/Tutorials/kinetic/No%20Ros%20Installation>).
This consists of using a Maven repository (<https://github.com/rosjava/rosjava_mvn_repo/raw/master>) to download pre-built jars for ROS 0.3.6.

### Complications

For the project Mindbot, the VSM plugin must communicate with the ROS-machine through custom messages and services which are not available in the default pre-compiled ROS jars.
For this reason, it is necessary to install rosjava on the ROS-machine and compile a java project able to convert the `mindbot_msgs` custom packages into java classes.
The create package is then manually copied as `lib/mindbot_msgs-0.0.0.jar`.

Instructions on how to build such package are given later

## Running

You probably need to first edit the IP address of the ROS-machine in the `build.gradle` file:

```
run {
    args = ["http://192.168.56.101:11311"]
}
```


The, run this project as a standalone project and run the test using the `run` gradle task.

    > ./gradlew run


## Building mindbot_msgs-x.y.z.jar
First create a folder, name it how you like, for this tutorial we name it 'MindbotCommunication'. 
Then open a terminal and move to that folder. 
With the following commands we create a source folder and rosjava creates for us a package and a project which contains a Subscriber and a Publisher:
```
mkdir -p src
cd src
catkin_create_rosjava_pkg communication_package
cd ..
catkin_make
source devel/setup.bash
cd src/communication_package
catkin_create_rosjava_project communication_project
cd ../..
catkin_make
```

Catkin_make builds the project and creates a build and a devel folder.

We have created a Project which contains a subscriber and a publisher. Now we need to move the mindbot_msgs into the source folder which is in the outermost folder MindbotCommunication where also the folder communication_package is located.

Now you need to change the following files (replace the methods with the following methods):

src/communication_package/CMakeLists.txt:
```
find_package(catkin REQUIRED
rosjava_build_tools
message_generation
mindbot_msgs)
catkin_package(CATKIN_DEPENDS
message_runtime
mindbot_msgs)
```

src/communcation_package/package.xml:
```
<buildtool_depend>catkin</buildtool_depend>
<build_depend>rosjava_build_tools</build_depend>
<build_depend>message_generation</build_depend>
<build_depend>mindbot_msgs</build_depend>
<exec_depend>mindbot_msgs</exec_depend>
<exec_depend>message_runtime</exec_depend>
```

src/communcation_package/communcation_project/build.gradle:
```
compile 'org.ros.rosjava_core:rosjava:[0.3,0.4)'
compile 'org.ros.rosjava_messages:mindbot_msgs:[0.0, 0.1)'
```

Once again in a console move to the folder MindbotCommunication and:
```
catkin_make
```

You can find the jar in MindbotCommunication/build/mindbot_msgs/java/mindbot_msgs/build/libs