; Auto-generated. Do not edit!


(cl:in-package mindbot_msgs-msg)


;//! \htmlinclude CtrlMode.msg.html

(cl:defclass <CtrlMode> (roslisp-msg-protocol:ros-message)
  ((ctrl_mode
    :reader ctrl_mode
    :initarg :ctrl_mode
    :type cl:fixnum
    :initform 0))
)

(cl:defclass CtrlMode (<CtrlMode>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <CtrlMode>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'CtrlMode)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-msg:<CtrlMode> is deprecated: use mindbot_msgs-msg:CtrlMode instead.")))

(cl:ensure-generic-function 'ctrl_mode-val :lambda-list '(m))
(cl:defmethod ctrl_mode-val ((m <CtrlMode>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-msg:ctrl_mode-val is deprecated.  Use mindbot_msgs-msg:ctrl_mode instead.")
  (ctrl_mode m))
(cl:defmethod roslisp-msg-protocol:symbol-codes ((msg-type (cl:eql '<CtrlMode>)))
    "Constants for message type '<CtrlMode>"
  '((:MODE0 . 0)
    (:MODE1 . 1)
    (:MODE2 . 2))
)
(cl:defmethod roslisp-msg-protocol:symbol-codes ((msg-type (cl:eql 'CtrlMode)))
    "Constants for message type 'CtrlMode"
  '((:MODE0 . 0)
    (:MODE1 . 1)
    (:MODE2 . 2))
)
(cl:defmethod roslisp-msg-protocol:serialize ((msg <CtrlMode>) ostream)
  "Serializes a message object of type '<CtrlMode>"
  (cl:write-byte (cl:ldb (cl:byte 8 0) (cl:slot-value msg 'ctrl_mode)) ostream)
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <CtrlMode>) istream)
  "Deserializes a message object of type '<CtrlMode>"
    (cl:setf (cl:ldb (cl:byte 8 0) (cl:slot-value msg 'ctrl_mode)) (cl:read-byte istream))
  msg
)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<CtrlMode>)))
  "Returns string type for a message object of type '<CtrlMode>"
  "mindbot_msgs/CtrlMode")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'CtrlMode)))
  "Returns string type for a message object of type 'CtrlMode"
  "mindbot_msgs/CtrlMode")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<CtrlMode>)))
  "Returns md5sum for a message object of type '<CtrlMode>"
  "e5e929f57b05b5ae4f0748d62736ba48")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'CtrlMode)))
  "Returns md5sum for a message object of type 'CtrlMode"
  "e5e929f57b05b5ae4f0748d62736ba48")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<CtrlMode>)))
  "Returns full string definition for message of type '<CtrlMode>"
  (cl:format cl:nil "#CtrlMode message~%~%#List of the available control modes~%uint8 MODE0 = 0~%uint8 MODE1 = 1~%uint8 MODE2 = 2~%~%#Assigned control mode~%uint8 ctrl_mode~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'CtrlMode)))
  "Returns full string definition for message of type 'CtrlMode"
  (cl:format cl:nil "#CtrlMode message~%~%#List of the available control modes~%uint8 MODE0 = 0~%uint8 MODE1 = 1~%uint8 MODE2 = 2~%~%#Assigned control mode~%uint8 ctrl_mode~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <CtrlMode>))
  (cl:+ 0
     1
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <CtrlMode>))
  "Converts a ROS message object to a list"
  (cl:list 'CtrlMode
    (cl:cons ':ctrl_mode (ctrl_mode msg))
))
