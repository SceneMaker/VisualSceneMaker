;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::CtrlState)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'CtrlState (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::CTRLSTATE")
  (make-package "MINDBOT_MSGS::CTRLSTATE"))

(in-package "ROS")
;;//! \htmlinclude CtrlState.msg.html


(intern "*OFF*" (find-package "MINDBOT_MSGS::CTRLSTATE"))
(shadow '*OFF* (find-package "MINDBOT_MSGS::CTRLSTATE"))
(defconstant mindbot_msgs::CtrlState::*OFF* 0)
(intern "*ON*" (find-package "MINDBOT_MSGS::CTRLSTATE"))
(shadow '*ON* (find-package "MINDBOT_MSGS::CTRLSTATE"))
(defconstant mindbot_msgs::CtrlState::*ON* 1)
(intern "*ERROR*" (find-package "MINDBOT_MSGS::CTRLSTATE"))
(shadow '*ERROR* (find-package "MINDBOT_MSGS::CTRLSTATE"))
(defconstant mindbot_msgs::CtrlState::*ERROR* 2)
(defclass mindbot_msgs::CtrlState
  :super ros::object
  :slots (_ctrl_state ))

(defmethod mindbot_msgs::CtrlState
  (:init
   (&key
    ((:ctrl_state __ctrl_state) 0)
    )
   (send-super :init)
   (setq _ctrl_state (round __ctrl_state))
   self)
  (:ctrl_state
   (&optional __ctrl_state)
   (if __ctrl_state (setq _ctrl_state __ctrl_state)) _ctrl_state)
  (:serialization-length
   ()
   (+
    ;; uint8 _ctrl_state
    1
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; uint8 _ctrl_state
       (write-byte _ctrl_state s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; uint8 _ctrl_state
     (setq _ctrl_state (sys::peek buf ptr- :char)) (incf ptr- 1)
   ;;
   self)
  )

(setf (get mindbot_msgs::CtrlState :md5sum-) "9bd2ce98651f7da53d3394dd6a9978bc")
(setf (get mindbot_msgs::CtrlState :datatype-) "mindbot_msgs/CtrlState")
(setf (get mindbot_msgs::CtrlState :definition-)
      "#CtrlState message

#List of the available control states
uint8 OFF = 0
uint8 ON = 1
uint8 ERROR = 2

#Assigned control state
uint8 ctrl_state
")



(provide :mindbot_msgs/CtrlState "9bd2ce98651f7da53d3394dd6a9978bc")


