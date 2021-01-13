;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::CtrlMode)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'CtrlMode (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::CTRLMODE")
  (make-package "MINDBOT_MSGS::CTRLMODE"))

(in-package "ROS")
;;//! \htmlinclude CtrlMode.msg.html


(intern "*MODE0*" (find-package "MINDBOT_MSGS::CTRLMODE"))
(shadow '*MODE0* (find-package "MINDBOT_MSGS::CTRLMODE"))
(defconstant mindbot_msgs::CtrlMode::*MODE0* 0)
(intern "*MODE1*" (find-package "MINDBOT_MSGS::CTRLMODE"))
(shadow '*MODE1* (find-package "MINDBOT_MSGS::CTRLMODE"))
(defconstant mindbot_msgs::CtrlMode::*MODE1* 1)
(intern "*MODE2*" (find-package "MINDBOT_MSGS::CTRLMODE"))
(shadow '*MODE2* (find-package "MINDBOT_MSGS::CTRLMODE"))
(defconstant mindbot_msgs::CtrlMode::*MODE2* 2)
(defclass mindbot_msgs::CtrlMode
  :super ros::object
  :slots (_ctrl_mode ))

(defmethod mindbot_msgs::CtrlMode
  (:init
   (&key
    ((:ctrl_mode __ctrl_mode) 0)
    )
   (send-super :init)
   (setq _ctrl_mode (round __ctrl_mode))
   self)
  (:ctrl_mode
   (&optional __ctrl_mode)
   (if __ctrl_mode (setq _ctrl_mode __ctrl_mode)) _ctrl_mode)
  (:serialization-length
   ()
   (+
    ;; uint8 _ctrl_mode
    1
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; uint8 _ctrl_mode
       (write-byte _ctrl_mode s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; uint8 _ctrl_mode
     (setq _ctrl_mode (sys::peek buf ptr- :char)) (incf ptr- 1)
   ;;
   self)
  )

(setf (get mindbot_msgs::CtrlMode :md5sum-) "e5e929f57b05b5ae4f0748d62736ba48")
(setf (get mindbot_msgs::CtrlMode :datatype-) "mindbot_msgs/CtrlMode")
(setf (get mindbot_msgs::CtrlMode :definition-)
      "#CtrlMode message

#List of the available control modes
uint8 MODE0 = 0
uint8 MODE1 = 1
uint8 MODE2 = 2

#Assigned control mode
uint8 ctrl_mode
")



(provide :mindbot_msgs/CtrlMode "e5e929f57b05b5ae4f0748d62736ba48")


