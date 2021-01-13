;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::SetCtrlMode)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'SetCtrlMode (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::SETCTRLMODE")
  (make-package "MINDBOT_MSGS::SETCTRLMODE"))
(unless (find-package "MINDBOT_MSGS::SETCTRLMODEREQUEST")
  (make-package "MINDBOT_MSGS::SETCTRLMODEREQUEST"))
(unless (find-package "MINDBOT_MSGS::SETCTRLMODERESPONSE")
  (make-package "MINDBOT_MSGS::SETCTRLMODERESPONSE"))

(in-package "ROS")





(defclass mindbot_msgs::SetCtrlModeRequest
  :super ros::object
  :slots (_ctrl_mode ))

(defmethod mindbot_msgs::SetCtrlModeRequest
  (:init
   (&key
    ((:ctrl_mode __ctrl_mode) (instance mindbot_msgs::CtrlMode :init))
    )
   (send-super :init)
   (setq _ctrl_mode __ctrl_mode)
   self)
  (:ctrl_mode
   (&rest __ctrl_mode)
   (if (keywordp (car __ctrl_mode))
       (send* _ctrl_mode __ctrl_mode)
     (progn
       (if __ctrl_mode (setq _ctrl_mode (car __ctrl_mode)))
       _ctrl_mode)))
  (:serialization-length
   ()
   (+
    ;; mindbot_msgs/CtrlMode _ctrl_mode
    (send _ctrl_mode :serialization-length)
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; mindbot_msgs/CtrlMode _ctrl_mode
       (send _ctrl_mode :serialize s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; mindbot_msgs/CtrlMode _ctrl_mode
     (send _ctrl_mode :deserialize buf ptr-) (incf ptr- (send _ctrl_mode :serialization-length))
   ;;
   self)
  )

(defclass mindbot_msgs::SetCtrlModeResponse
  :super ros::object
  :slots (_success _message ))

(defmethod mindbot_msgs::SetCtrlModeResponse
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

(defclass mindbot_msgs::SetCtrlMode
  :super ros::object
  :slots ())

(setf (get mindbot_msgs::SetCtrlMode :md5sum-) "f628f1e6fadee58433a3f4d581cef831")
(setf (get mindbot_msgs::SetCtrlMode :datatype-) "mindbot_msgs/SetCtrlMode")
(setf (get mindbot_msgs::SetCtrlMode :request) mindbot_msgs::SetCtrlModeRequest)
(setf (get mindbot_msgs::SetCtrlMode :response) mindbot_msgs::SetCtrlModeResponse)

(defmethod mindbot_msgs::SetCtrlModeRequest
  (:response () (instance mindbot_msgs::SetCtrlModeResponse :init)))

(setf (get mindbot_msgs::SetCtrlModeRequest :md5sum-) "f628f1e6fadee58433a3f4d581cef831")
(setf (get mindbot_msgs::SetCtrlModeRequest :datatype-) "mindbot_msgs/SetCtrlModeRequest")
(setf (get mindbot_msgs::SetCtrlModeRequest :definition-)
      "#SetCtrlMode service

#REQUEST
mindbot_msgs/CtrlMode ctrl_mode


================================================================================
MSG: mindbot_msgs/CtrlMode
#CtrlMode message

#List of the available control modes
uint8 MODE0 = 0
uint8 MODE1 = 1
uint8 MODE2 = 2

#Assigned control mode
---

#RESPONSE
bool success
string message

")

(setf (get mindbot_msgs::SetCtrlModeResponse :md5sum-) "f628f1e6fadee58433a3f4d581cef831")
(setf (get mindbot_msgs::SetCtrlModeResponse :datatype-) "mindbot_msgs/SetCtrlModeResponse")
(setf (get mindbot_msgs::SetCtrlModeResponse :definition-)
      "#SetCtrlMode service

#REQUEST
mindbot_msgs/CtrlMode ctrl_mode


================================================================================
MSG: mindbot_msgs/CtrlMode
#CtrlMode message

#List of the available control modes
uint8 MODE0 = 0
uint8 MODE1 = 1
uint8 MODE2 = 2

#Assigned control mode
---

#RESPONSE
bool success
string message

")



(provide :mindbot_msgs/SetCtrlMode "f628f1e6fadee58433a3f4d581cef831")


