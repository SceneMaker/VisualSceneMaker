;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::SetJointState)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'SetJointState (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::SETJOINTSTATE")
  (make-package "MINDBOT_MSGS::SETJOINTSTATE"))
(unless (find-package "MINDBOT_MSGS::SETJOINTSTATEREQUEST")
  (make-package "MINDBOT_MSGS::SETJOINTSTATEREQUEST"))
(unless (find-package "MINDBOT_MSGS::SETJOINTSTATERESPONSE")
  (make-package "MINDBOT_MSGS::SETJOINTSTATERESPONSE"))

(in-package "ROS")

(if (not (find-package "SENSOR_MSGS"))
  (ros::roseus-add-msgs "sensor_msgs"))




(defclass mindbot_msgs::SetJointStateRequest
  :super ros::object
  :slots (_point ))

(defmethod mindbot_msgs::SetJointStateRequest
  (:init
   (&key
    ((:point __point) (instance sensor_msgs::JointState :init))
    )
   (send-super :init)
   (setq _point __point)
   self)
  (:point
   (&rest __point)
   (if (keywordp (car __point))
       (send* _point __point)
     (progn
       (if __point (setq _point (car __point)))
       _point)))
  (:serialization-length
   ()
   (+
    ;; sensor_msgs/JointState _point
    (send _point :serialization-length)
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; sensor_msgs/JointState _point
       (send _point :serialize s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; sensor_msgs/JointState _point
     (send _point :deserialize buf ptr-) (incf ptr- (send _point :serialization-length))
   ;;
   self)
  )

(defclass mindbot_msgs::SetJointStateResponse
  :super ros::object
  :slots (_success _message ))

(defmethod mindbot_msgs::SetJointStateResponse
  (:init
   (&key
    ((:success __success) nil)
    ((:message __message) "")
    )
   (send-super :init)
   (setq _success __success)
   (setq _message (string __message))
   self)
  (:success
   (&optional __success)
   (if __success (setq _success __success)) _success)
  (:message
   (&optional __message)
   (if __message (setq _message __message)) _message)
  (:serialization-length
   ()
   (+
    ;; bool _success
    1
    ;; string _message
    4 (length _message)
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; bool _success
       (if _success (write-byte -1 s) (write-byte 0 s))
     ;; string _message
       (write-long (length _message) s) (princ _message s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; bool _success
     (setq _success (not (= 0 (sys::peek buf ptr- :char)))) (incf ptr- 1)
   ;; string _message
     (let (n) (setq n (sys::peek buf ptr- :integer)) (incf ptr- 4) (setq _message (subseq buf ptr- (+ ptr- n))) (incf ptr- n))
   ;;
   self)
  )

(defclass mindbot_msgs::SetJointState
  :super ros::object
  :slots ())

(setf (get mindbot_msgs::SetJointState :md5sum-) "2dd47abbafae7d0f1f325f1e048e28c7")
(setf (get mindbot_msgs::SetJointState :datatype-) "mindbot_msgs/SetJointState")
(setf (get mindbot_msgs::SetJointState :request) mindbot_msgs::SetJointStateRequest)
(setf (get mindbot_msgs::SetJointState :response) mindbot_msgs::SetJointStateResponse)

(defmethod mindbot_msgs::SetJointStateRequest
  (:response () (instance mindbot_msgs::SetJointStateResponse :init)))

(setf (get mindbot_msgs::SetJointStateRequest :md5sum-) "2dd47abbafae7d0f1f325f1e048e28c7")
(setf (get mindbot_msgs::SetJointStateRequest :datatype-) "mindbot_msgs/SetJointStateRequest")
(setf (get mindbot_msgs::SetJointStateRequest :definition-)
      "#SetJointState service

#REQUEST
sensor_msgs/JointState point


================================================================================
MSG: sensor_msgs/JointState
# This is a message that holds data to describe the state of a set of torque controlled joints. 
#
# The state of each joint (revolute or prismatic) is defined by:
#  * the position of the joint (rad or m),
#  * the velocity of the joint (rad/s or m/s) and 
#  * the effort that is applied in the joint (Nm or N).
#
# Each joint is uniquely identified by its name
# The header specifies the time at which the joint states were recorded. All the joint states
# in one message have to be recorded at the same time.
#
# This message consists of a multiple arrays, one for each part of the joint state. 
# The goal is to make each of the fields optional. When e.g. your joints have no
# effort associated with them, you can leave the effort array empty. 
#
# All arrays in this message should have the same size, or be empty.
# This is the only way to uniquely associate the joint name with the correct
# states.


Header header

string[] name
float64[] position
float64[] velocity
float64[] effort

================================================================================
MSG: std_msgs/Header
# Standard metadata for higher-level stamped data types.
# This is generally used to communicate timestamped data 
# in a particular coordinate frame.
# 
# sequence ID: consecutively increasing ID 
uint32 seq
#Two-integer timestamp that is expressed as:
# * stamp.sec: seconds (stamp_secs) since epoch (in Python the variable is called 'secs')
# * stamp.nsec: nanoseconds since stamp_secs (in Python the variable is called 'nsecs')
# time-handling sugar is provided by the client library
time stamp
#Frame this data is associated with
string frame_id
---

#RESPONSE
bool success
string message

")

(setf (get mindbot_msgs::SetJointStateResponse :md5sum-) "2dd47abbafae7d0f1f325f1e048e28c7")
(setf (get mindbot_msgs::SetJointStateResponse :datatype-) "mindbot_msgs/SetJointStateResponse")
(setf (get mindbot_msgs::SetJointStateResponse :definition-)
      "#SetJointState service

#REQUEST
sensor_msgs/JointState point


================================================================================
MSG: sensor_msgs/JointState
# This is a message that holds data to describe the state of a set of torque controlled joints. 
#
# The state of each joint (revolute or prismatic) is defined by:
#  * the position of the joint (rad or m),
#  * the velocity of the joint (rad/s or m/s) and 
#  * the effort that is applied in the joint (Nm or N).
#
# Each joint is uniquely identified by its name
# The header specifies the time at which the joint states were recorded. All the joint states
# in one message have to be recorded at the same time.
#
# This message consists of a multiple arrays, one for each part of the joint state. 
# The goal is to make each of the fields optional. When e.g. your joints have no
# effort associated with them, you can leave the effort array empty. 
#
# All arrays in this message should have the same size, or be empty.
# This is the only way to uniquely associate the joint name with the correct
# states.


Header header

string[] name
float64[] position
float64[] velocity
float64[] effort

================================================================================
MSG: std_msgs/Header
# Standard metadata for higher-level stamped data types.
# This is generally used to communicate timestamped data 
# in a particular coordinate frame.
# 
# sequence ID: consecutively increasing ID 
uint32 seq
#Two-integer timestamp that is expressed as:
# * stamp.sec: seconds (stamp_secs) since epoch (in Python the variable is called 'secs')
# * stamp.nsec: nanoseconds since stamp_secs (in Python the variable is called 'nsecs')
# time-handling sugar is provided by the client library
time stamp
#Frame this data is associated with
string frame_id
---

#RESPONSE
bool success
string message

")



(provide :mindbot_msgs/SetJointState "2dd47abbafae7d0f1f325f1e048e28c7")


