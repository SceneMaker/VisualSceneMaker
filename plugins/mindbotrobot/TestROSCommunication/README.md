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

TODO (Sarah)

