;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::SetPose)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'SetPose (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::SETPOSE")
  (make-package "MINDBOT_MSGS::SETPOSE"))
(unless (find-package "MINDBOT_MSGS::SETPOSEREQUEST")
  (make-package "MINDBOT_MSGS::SETPOSEREQUEST"))
(unless (find-package "MINDBOT_MSGS::SETPOSERESPONSE")
  (make-package "MINDBOT_MSGS::SETPOSERESPONSE"))

(in-package "ROS")

(if (not (find-package "GEOMETRY_MSGS"))
  (ros::roseus-add-msgs "geometry_msgs"))




(defclass mindbot_msgs::SetPoseRequest
  :super ros::object
  :slots (_pose ))

(defmethod mindbot_msgs::SetPoseRequest
  (:init
   (&key
    ((:pose __pose) (instance geometry_msgs::Pose :init))
    )
   (send-super :init)
   (setq _pose __pose)
   self)
  (:pose
   (&rest __pose)
   (if (keywordp (car __pose))
       (send* _pose __pose)
     (progn
       (if __pose (setq _pose (car __pose)))
       _pose)))
  (:serialization-length
   ()
   (+
    ;; geometry_msgs/Pose _pose
    (send _pose :serialization-length)
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; geometry_msgs/Pose _pose
       (send _pose :serialize s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; geometry_msgs/Pose _pose
     (send _pose :deserialize buf ptr-) (incf ptr- (send _pose :serialization-length))
   ;;
   self)
  )

(defclass mindbot_msgs::SetPoseResponse
  :super ros::object
  :slots (_success _message ))

(defmethod mindbot_msgs::SetPoseResponse
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

(defclass mindbot_msgs::SetPose
  :super ros::object
  :slots ())

(setf (get mindbot_msgs::SetPose :md5sum-) "28e4dd667b29bd35b516ba1d379b7529")
(setf (get mindbot_msgs::SetPose :datatype-) "mindbot_msgs/SetPose")
(setf (get mindbot_msgs::SetPose :request) mindbot_msgs::SetPoseRequest)
(setf (get mindbot_msgs::SetPose :response) mindbot_msgs::SetPoseResponse)

(defmethod mindbot_msgs::SetPoseRequest
  (:response () (instance mindbot_msgs::SetPoseResponse :init)))

(setf (get mindbot_msgs::SetPoseRequest :md5sum-) "28e4dd667b29bd35b516ba1d379b7529")
(setf (get mindbot_msgs::SetPoseRequest :datatype-) "mindbot_msgs/SetPoseRequest")
(setf (get mindbot_msgs::SetPoseRequest :definition-)
      "#SetPose service

#REQUEST
geometry_msgs/Pose pose


================================================================================
MSG: geometry_msgs/Pose
# A representation of pose in free space, composed of position and orientation. 
Point position
Quaternion orientation

================================================================================
MSG: geometry_msgs/Point
# This contains the position of a point in free space
float64 x
float64 y
float64 z

================================================================================
MSG: geometry_msgs/Quaternion
# This represents an orientation in free space in quaternion form.

float64 x
float64 y
float64 z
float64 w
---

#RESPONSE
bool success
string message

")

(setf (get mindbot_msgs::SetPoseResponse :md5sum-) "28e4dd667b29bd35b516ba1d379b7529")
(setf (get mindbot_msgs::SetPoseResponse :datatype-) "mindbot_msgs/SetPoseResponse")
(setf (get mindbot_msgs::SetPoseResponse :definition-)
      "#SetPose service

#REQUEST
geometry_msgs/Pose pose


================================================================================
MSG: geometry_msgs/Pose
# A representation of pose in free space, composed of position and orientation. 
Point position
Quaternion orientation

================================================================================
MSG: geometry_msgs/Point
# This contains the position of a point in free space
float64 x
float64 y
float64 z

================================================================================
MSG: geometry_msgs/Quaternion
# This represents an orientation in free space in quaternion form.

float64 x
float64 y
float64 z
float64 w
---

#RESPONSE
bool success
string message

")



(provide :mindbot_msgs/SetPose "28e4dd667b29bd35b516ba1d379b7529")


