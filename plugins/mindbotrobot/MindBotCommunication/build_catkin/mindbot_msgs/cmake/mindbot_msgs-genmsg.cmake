# generated from genmsg/cmake/pkg-genmsg.cmake.em

message(STATUS "mindbot_msgs: 2 messages, 5 services")

set(MSG_I_FLAGS "-Imindbot_msgs:/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg;-Igeometry_msgs:/opt/ros/melodic/share/geometry_msgs/cmake/../msg;-Isensor_msgs:/opt/ros/melodic/share/sensor_msgs/cmake/../msg;-Istd_msgs:/opt/ros/melodic/share/std_msgs/cmake/../msg")

# Find all generators
find_package(gencpp REQUIRED)
find_package(geneus REQUIRED)
find_package(genjava REQUIRED)
find_package(genlisp REQUIRED)
find_package(gennodejs REQUIRED)
find_package(genpy REQUIRED)

add_custom_target(mindbot_msgs_generate_messages ALL)

# verify that message/service dependencies have not changed since configure



get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" "mindbot_msgs/CtrlMode"
)

get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" ""
)

get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" "geometry_msgs/Pose:geometry_msgs/Quaternion:geometry_msgs/Point"
)

get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" ""
)

get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" "geometry_msgs/Vector3"
)

get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" "sensor_msgs/JointState:std_msgs/Header"
)

get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_custom_target(_mindbot_msgs_generate_messages_check_deps_${_filename}
  COMMAND ${CATKIN_ENV} ${PYTHON_EXECUTABLE} ${GENMSG_CHECK_DEPS_SCRIPT} "mindbot_msgs" "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" "mindbot_msgs/CtrlState"
)

#
#  langs = gencpp;geneus;genjava;genlisp;gennodejs;genpy
#

### Section generating for lang: gencpp
### Generating Messages
_generate_msg_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)
_generate_msg_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)

### Generating Services
_generate_srv_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Vector3.msg"
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Pose.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Quaternion.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Point.msg"
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_cpp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/sensor_msgs/cmake/../msg/JointState.msg;/opt/ros/melodic/share/std_msgs/cmake/../msg/Header.msg"
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
)

### Generating Module File
_generate_module_cpp(mindbot_msgs
  ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
  "${ALL_GEN_OUTPUT_FILES_cpp}"
)

add_custom_target(mindbot_msgs_generate_messages_cpp
  DEPENDS ${ALL_GEN_OUTPUT_FILES_cpp}
)
add_dependencies(mindbot_msgs_generate_messages mindbot_msgs_generate_messages_cpp)

# add dependencies to all check dependencies targets
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_cpp _mindbot_msgs_generate_messages_check_deps_${_filename})

# target for backward compatibility
add_custom_target(mindbot_msgs_gencpp)
add_dependencies(mindbot_msgs_gencpp mindbot_msgs_generate_messages_cpp)

# register target for catkin_package(EXPORTED_TARGETS)
list(APPEND ${PROJECT_NAME}_EXPORTED_TARGETS mindbot_msgs_generate_messages_cpp)

### Section generating for lang: geneus
### Generating Messages
_generate_msg_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)
_generate_msg_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)

### Generating Services
_generate_srv_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Vector3.msg"
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Pose.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Quaternion.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Point.msg"
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_eus(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/sensor_msgs/cmake/../msg/JointState.msg;/opt/ros/melodic/share/std_msgs/cmake/../msg/Header.msg"
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
)

### Generating Module File
_generate_module_eus(mindbot_msgs
  ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
  "${ALL_GEN_OUTPUT_FILES_eus}"
)

add_custom_target(mindbot_msgs_generate_messages_eus
  DEPENDS ${ALL_GEN_OUTPUT_FILES_eus}
)
add_dependencies(mindbot_msgs_generate_messages mindbot_msgs_generate_messages_eus)

# add dependencies to all check dependencies targets
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_eus _mindbot_msgs_generate_messages_check_deps_${_filename})

# target for backward compatibility
add_custom_target(mindbot_msgs_geneus)
add_dependencies(mindbot_msgs_geneus mindbot_msgs_generate_messages_eus)

# register target for catkin_package(EXPORTED_TARGETS)
list(APPEND ${PROJECT_NAME}_EXPORTED_TARGETS mindbot_msgs_generate_messages_eus)

### Section generating for lang: genjava
### Generating Messages
_generate_msg_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)
_generate_msg_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)

### Generating Services
_generate_srv_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Vector3.msg"
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Pose.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Quaternion.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Point.msg"
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_java(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/sensor_msgs/cmake/../msg/JointState.msg;/opt/ros/melodic/share/std_msgs/cmake/../msg/Header.msg"
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
)

### Generating Module File
_generate_module_java(mindbot_msgs
  ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
  "${ALL_GEN_OUTPUT_FILES_java}"
)

add_custom_target(mindbot_msgs_generate_messages_java
  DEPENDS ${ALL_GEN_OUTPUT_FILES_java}
)
add_dependencies(mindbot_msgs_generate_messages mindbot_msgs_generate_messages_java)

# add dependencies to all check dependencies targets
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_java _mindbot_msgs_generate_messages_check_deps_${_filename})

# target for backward compatibility
add_custom_target(mindbot_msgs_genjava)
add_dependencies(mindbot_msgs_genjava mindbot_msgs_generate_messages_java)

# register target for catkin_package(EXPORTED_TARGETS)
list(APPEND ${PROJECT_NAME}_EXPORTED_TARGETS mindbot_msgs_generate_messages_java)

### Section generating for lang: genlisp
### Generating Messages
_generate_msg_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)
_generate_msg_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)

### Generating Services
_generate_srv_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Vector3.msg"
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Pose.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Quaternion.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Point.msg"
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_lisp(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/sensor_msgs/cmake/../msg/JointState.msg;/opt/ros/melodic/share/std_msgs/cmake/../msg/Header.msg"
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
)

### Generating Module File
_generate_module_lisp(mindbot_msgs
  ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
  "${ALL_GEN_OUTPUT_FILES_lisp}"
)

add_custom_target(mindbot_msgs_generate_messages_lisp
  DEPENDS ${ALL_GEN_OUTPUT_FILES_lisp}
)
add_dependencies(mindbot_msgs_generate_messages mindbot_msgs_generate_messages_lisp)

# add dependencies to all check dependencies targets
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_lisp _mindbot_msgs_generate_messages_check_deps_${_filename})

# target for backward compatibility
add_custom_target(mindbot_msgs_genlisp)
add_dependencies(mindbot_msgs_genlisp mindbot_msgs_generate_messages_lisp)

# register target for catkin_package(EXPORTED_TARGETS)
list(APPEND ${PROJECT_NAME}_EXPORTED_TARGETS mindbot_msgs_generate_messages_lisp)

### Section generating for lang: gennodejs
### Generating Messages
_generate_msg_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)
_generate_msg_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)

### Generating Services
_generate_srv_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Vector3.msg"
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Pose.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Quaternion.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Point.msg"
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_nodejs(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/sensor_msgs/cmake/../msg/JointState.msg;/opt/ros/melodic/share/std_msgs/cmake/../msg/Header.msg"
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
)

### Generating Module File
_generate_module_nodejs(mindbot_msgs
  ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
  "${ALL_GEN_OUTPUT_FILES_nodejs}"
)

add_custom_target(mindbot_msgs_generate_messages_nodejs
  DEPENDS ${ALL_GEN_OUTPUT_FILES_nodejs}
)
add_dependencies(mindbot_msgs_generate_messages mindbot_msgs_generate_messages_nodejs)

# add dependencies to all check dependencies targets
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_nodejs _mindbot_msgs_generate_messages_check_deps_${_filename})

# target for backward compatibility
add_custom_target(mindbot_msgs_gennodejs)
add_dependencies(mindbot_msgs_gennodejs mindbot_msgs_generate_messages_nodejs)

# register target for catkin_package(EXPORTED_TARGETS)
list(APPEND ${PROJECT_NAME}_EXPORTED_TARGETS mindbot_msgs_generate_messages_nodejs)

### Section generating for lang: genpy
### Generating Messages
_generate_msg_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)
_generate_msg_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  "${MSG_I_FLAGS}"
  ""
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)

### Generating Services
_generate_srv_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg"
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv"
  "${MSG_I_FLAGS}"
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg"
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Vector3.msg"
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Pose.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Quaternion.msg;/opt/ros/melodic/share/geometry_msgs/cmake/../msg/Point.msg"
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)
_generate_srv_py(mindbot_msgs
  "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv"
  "${MSG_I_FLAGS}"
  "/opt/ros/melodic/share/sensor_msgs/cmake/../msg/JointState.msg;/opt/ros/melodic/share/std_msgs/cmake/../msg/Header.msg"
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
)

### Generating Module File
_generate_module_py(mindbot_msgs
  ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
  "${ALL_GEN_OUTPUT_FILES_py}"
)

add_custom_target(mindbot_msgs_generate_messages_py
  DEPENDS ${ALL_GEN_OUTPUT_FILES_py}
)
add_dependencies(mindbot_msgs_generate_messages mindbot_msgs_generate_messages_py)

# add dependencies to all check dependencies targets
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlMode.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlMode.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetPose.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/msg/CtrlState.msg" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetVector3.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetJointState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})
get_filename_component(_filename "/home/sarah/MindbotRobot/VisualSceneMaker/plugins/mindbotrobot/MindBotCommunication/src/mindbot_msgs/srv/SetCtrlState.srv" NAME_WE)
add_dependencies(mindbot_msgs_generate_messages_py _mindbot_msgs_generate_messages_check_deps_${_filename})

# target for backward compatibility
add_custom_target(mindbot_msgs_genpy)
add_dependencies(mindbot_msgs_genpy mindbot_msgs_generate_messages_py)

# register target for catkin_package(EXPORTED_TARGETS)
list(APPEND ${PROJECT_NAME}_EXPORTED_TARGETS mindbot_msgs_generate_messages_py)



if(gencpp_INSTALL_DIR AND EXISTS ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs)
  # install generated code
  install(
    DIRECTORY ${CATKIN_DEVEL_PREFIX}/${gencpp_INSTALL_DIR}/mindbot_msgs
    DESTINATION ${gencpp_INSTALL_DIR}
  )
endif()
if(TARGET geometry_msgs_generate_messages_cpp)
  add_dependencies(mindbot_msgs_generate_messages_cpp geometry_msgs_generate_messages_cpp)
endif()
if(TARGET sensor_msgs_generate_messages_cpp)
  add_dependencies(mindbot_msgs_generate_messages_cpp sensor_msgs_generate_messages_cpp)
endif()

if(geneus_INSTALL_DIR AND EXISTS ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs)
  # install generated code
  install(
    DIRECTORY ${CATKIN_DEVEL_PREFIX}/${geneus_INSTALL_DIR}/mindbot_msgs
    DESTINATION ${geneus_INSTALL_DIR}
  )
endif()
if(TARGET geometry_msgs_generate_messages_eus)
  add_dependencies(mindbot_msgs_generate_messages_eus geometry_msgs_generate_messages_eus)
endif()
if(TARGET sensor_msgs_generate_messages_eus)
  add_dependencies(mindbot_msgs_generate_messages_eus sensor_msgs_generate_messages_eus)
endif()

if(genjava_INSTALL_DIR AND EXISTS ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs)
  # install generated code
  install(
    DIRECTORY ${CATKIN_DEVEL_PREFIX}/${genjava_INSTALL_DIR}/mindbot_msgs
    DESTINATION ${genjava_INSTALL_DIR}
  )
endif()
if(TARGET geometry_msgs_generate_messages_java)
  add_dependencies(mindbot_msgs_generate_messages_java geometry_msgs_generate_messages_java)
endif()
if(TARGET sensor_msgs_generate_messages_java)
  add_dependencies(mindbot_msgs_generate_messages_java sensor_msgs_generate_messages_java)
endif()

if(genlisp_INSTALL_DIR AND EXISTS ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs)
  # install generated code
  install(
    DIRECTORY ${CATKIN_DEVEL_PREFIX}/${genlisp_INSTALL_DIR}/mindbot_msgs
    DESTINATION ${genlisp_INSTALL_DIR}
  )
endif()
if(TARGET geometry_msgs_generate_messages_lisp)
  add_dependencies(mindbot_msgs_generate_messages_lisp geometry_msgs_generate_messages_lisp)
endif()
if(TARGET sensor_msgs_generate_messages_lisp)
  add_dependencies(mindbot_msgs_generate_messages_lisp sensor_msgs_generate_messages_lisp)
endif()

if(gennodejs_INSTALL_DIR AND EXISTS ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs)
  # install generated code
  install(
    DIRECTORY ${CATKIN_DEVEL_PREFIX}/${gennodejs_INSTALL_DIR}/mindbot_msgs
    DESTINATION ${gennodejs_INSTALL_DIR}
  )
endif()
if(TARGET geometry_msgs_generate_messages_nodejs)
  add_dependencies(mindbot_msgs_generate_messages_nodejs geometry_msgs_generate_messages_nodejs)
endif()
if(TARGET sensor_msgs_generate_messages_nodejs)
  add_dependencies(mindbot_msgs_generate_messages_nodejs sensor_msgs_generate_messages_nodejs)
endif()

if(genpy_INSTALL_DIR AND EXISTS ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs)
  install(CODE "execute_process(COMMAND \"/usr/bin/python2\" -m compileall \"${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs\")")
  # install generated code
  install(
    DIRECTORY ${CATKIN_DEVEL_PREFIX}/${genpy_INSTALL_DIR}/mindbot_msgs
    DESTINATION ${genpy_INSTALL_DIR}
  )
endif()
if(TARGET geometry_msgs_generate_messages_py)
  add_dependencies(mindbot_msgs_generate_messages_py geometry_msgs_generate_messages_py)
endif()
if(TARGET sensor_msgs_generate_messages_py)
  add_dependencies(mindbot_msgs_generate_messages_py sensor_msgs_generate_messages_py)
endif()
