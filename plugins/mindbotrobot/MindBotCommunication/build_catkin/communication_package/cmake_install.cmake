# Install script for directory: /home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/communication_package

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/communication_package/catkin_generated/installspace/communication_package.pc")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/communication_package/cmake" TYPE FILE FILES
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/communication_package/catkin_generated/installspace/communication_packageConfig.cmake"
    "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/build/communication_package/catkin_generated/installspace/communication_packageConfig-version.cmake"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/communication_package" TYPE FILE FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/communication_package/package.xml")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/maven/communication_package" TYPE DIRECTORY FILES "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/devel/share/maven/communication_package/")
endif()

