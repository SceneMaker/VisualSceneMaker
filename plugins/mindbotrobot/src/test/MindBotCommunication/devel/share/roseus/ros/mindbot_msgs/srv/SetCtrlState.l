;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::SetCtrlState)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'SetCtrlState (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::SETCTRLSTATE")
  (make-package "MINDBOT_MSGS::SETCTRLSTATE"))
(unless (find-package "MINDBOT_MSGS::SETCTRLSTATEREQUEST")
  (make-package "MINDBOT_MSGS::SETCTRLSTATEREQUEST"))
(unless (find-package "MINDBOT_MSGS::SETCTRLSTATERESPONSE")
  (make-package "MINDBOT_MSGS::SETCTRLSTATERESPONSE"))

(in-package "ROS")





(defclass mindbot_msgs::SetCtrlStateRequest
  :super ros::object
  :slots (_ctrl_state ))

(defmethod mindbot_msgs::SetCtrlStateRequest
  (:init
   (&key
    ((:ctrl_state __ctrl_state) (instance mindbot_msgs::CtrlState :init))
    )
   (send-super :init)
   (setq _ctrl_state __ctrl_state)
   self)
  (:ctrl_state
   (&rest __ctrl_state)
   (if (keywordp (car __ctrl_state))
       (send* _ctrl_state __ctrl_state)
     (progn
       (if __ctrl_state (setq _ctrl_state (car __ctrl_state)))
       _ctrl_state)))
  (:serialization-length
   ()
   (+
    ;; mindbot_msgs/CtrlState _ctrl_state
    (send _ctrl_state :serialization-length)
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; mindbot_msgs/CtrlState _ctrl_state
       (send _ctrl_state :serialize s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; mindbot_msgs/CtrlState _ctrl_state
     (send _ctrl_state :deserialize buf ptr-) (incf ptr- (send _ctrl_state :serialization-length))
   ;;
   self)
  )

(defclass mindbot_msgs::SetCtrlStateResponse
  :super ros::object
  :slots (_success _message ))

(defmethod mindbot_msgs::SetCtrlStateResponse
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

(defclass mindbot_msgs::SetCtrlState
  :super ros::object
  :slots ())

(setf (get mindbot_msgs::SetCtrlState :md5sum-) "60923bc5e5c8d30acc73ee0d6e56f3ab")
(setf (get mindbot_msgs::SetCtrlState :datatype-) "mindbot_msgs/SetCtrlState")
(setf (get mindbot_msgs::SetCtrlState :request) mindbot_msgs::SetCtrlStateRequest)
(setf (get mindbot_msgs::SetCtrlState :response) mindbot_msgs::SetCtrlStateResponse)

(defmethod mindbot_msgs::SetCtrlStateRequest
  (:response () (instance mindbot_msgs::SetCtrlStateResponse :init)))

(setf (get mindbot_msgs::SetCtrlStateRequest :md5sum-) "60923bc5e5c8d30acc73ee0d6e56f3ab")
(setf (get mindbot_msgs::SetCtrlStateRequest :datatype-) "mindbot_msgs/SetCtrlStateRequest")
(setf (get mindbot_msgs::SetCtrlStateRequest :definition-)
      "#SetCtrlState service

#REQUEST
mindbot_msgs/CtrlState ctrl_state


================================================================================
MSG: mindbot_msgs/CtrlState
#CtrlState message

#List of the available control states
uint8 OFF = 0
uint8 ON = 1
uint8 ERROR = 2

#Assigned control state
---

#RESPONSE
bool success
string message

")

(setf (get mindbot_msgs::SetCtrlStateResponse :md5sum-) "60923bc5e5c8d30acc73ee0d6e56f3ab")
(setf (get mindbot_msgs::SetCtrlStateResponse :datatype-) "mindbot_msgs/SetCtrlStateResponse")
(setf (get mindbot_msgs::SetCtrlStateResponse :definition-)
      "#SetCtrlState service

#REQUEST
mindbot_msgs/CtrlState ctrl_state


================================================================================
MSG: mindbot_msgs/CtrlState
#CtrlState message

#List of the available control states
uint8 OFF = 0
uint8 ON = 1
uint8 ERROR = 2

#Assigned control state
---

#RESPONSE
bool success
string message

")



(provide :mindbot_msgs/SetCtrlState "60923bc5e5c8d30acc73ee0d6e56f3ab")


