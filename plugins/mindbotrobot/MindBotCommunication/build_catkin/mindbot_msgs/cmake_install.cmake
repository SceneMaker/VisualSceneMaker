# Install script for directory: /home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/install")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/mindbot_msgs/msg" TYPE FILE FILES
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/mindbot_msgs/srv" TYPE FILE FILES
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/mindbot_msgs/cmake" TYPE FILE FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/mindbot_msgs/catkin_generated/installspace/mindbot_msgs-msg-paths.cmake")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE DIRECTORY FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/include/mindbot_msgs")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/roseus/ros" TYPE DIRECTORY FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/share/roseus/ros/mindbot_msgs")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/common-lisp/ros" TYPE DIRECTORY FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/share/common-lisp/ros/mindbot_msgs")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/gennodejs/ros" TYPE DIRECTORY FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/share/gennodejs/ros/mindbot_msgs")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  execute_process(COMMAND "/usr/bin/python2" -m compileall "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/lib/python2.7/dist-packages/mindbot_msgs")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/python2.7/dist-packages" TYPE DIRECTORY FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/lib/python2.7/dist-packages/mindbot_msgs")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/mindbot_msgs/catkin_generated/installspace/mindbot_msgs.pc")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/mindbot_msgs/cmake" TYPE FILE FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/mindbot_msgs/catkin_generated/installspace/mindbot_msgs-msg-extras.cmake")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/mindbot_msgs/cmake" TYPE FILE FILES
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/mindbot_msgs/catkin_generated/installspace/mindbot_msgsConfig.cmake"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/mindbot_msgs/catkin_generated/installspace/mindbot_msgsConfig-version.cmake"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/mindbot_msgs" TYPE FILE FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/package.xml")
endif()

