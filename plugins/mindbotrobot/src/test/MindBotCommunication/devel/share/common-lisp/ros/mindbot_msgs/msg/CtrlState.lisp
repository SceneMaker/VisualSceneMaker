; Auto-generated. Do not edit!


(cl:in-package mindbot_msgs-msg)


;//! \htmlinclude CtrlState.msg.html

(cl:defclass <CtrlState> (roslisp-msg-protocol:ros-message)
  ((ctrl_state
    :reader ctrl_state
    :initarg :ctrl_state
    :type cl:fixnum
    :initform 0))
)

(cl:defclass CtrlState (<CtrlState>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <CtrlState>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'CtrlState)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-msg:<CtrlState> is deprecated: use mindbot_msgs-msg:CtrlState instead.")))

(cl:ensure-generic-function 'ctrl_state-val :lambda-list '(m))
(cl:defmethod ctrl_state-val ((m <CtrlState>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-msg:ctrl_state-val is deprecated.  Use mindbot_msgs-msg:ctrl_state instead.")
  (ctrl_state m))
(cl:defmethod roslisp-msg-protocol:symbol-codes ((msg-type (cl:eql '<CtrlState>)))
    "Constants for message type '<CtrlState>"
  '((:OFF . 0)
    (:ON . 1)
    (:ERROR . 2))
)
(cl:defmethod roslisp-msg-protocol:symbol-codes ((msg-type (cl:eql 'CtrlState)))
    "Constants for message type 'CtrlState"
  '((:OFF . 0)
    (:ON . 1)
    (:ERROR . 2))
)
(cl:defmethod roslisp-msg-protocol:serialize ((msg <CtrlState>) ostream)
  "Serializes a message object of type '<CtrlState>"
  (cl:write-byte (cl:ldb (cl:byte 8 0) (cl:slot-value msg 'ctrl_state)) ostream)
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <CtrlState>) istream)
  "Deserializes a message object of type '<CtrlState>"
    (cl:setf (cl:ldb (cl:byte 8 0) (cl:slot-value msg 'ctrl_state)) (cl:read-byte istream))
  msg
)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<CtrlState>)))
  "Returns string type for a message object of type '<CtrlState>"
  "mindbot_msgs/CtrlState")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'CtrlState)))
  "Returns string type for a message object of type 'CtrlState"
  "mindbot_msgs/CtrlState")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<CtrlState>)))
  "Returns md5sum for a message object of type '<CtrlState>"
  "9bd2ce98651f7da53d3394dd6a9978bc")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'CtrlState)))
  "Returns md5sum for a message object of type 'CtrlState"
  "9bd2ce98651f7da53d3394dd6a9978bc")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<CtrlState>)))
  "Returns full string definition for message of type '<CtrlState>"
  (cl:format cl:nil "#CtrlState message~%~%#List of the available control states~%uint8 OFF = 0~%uint8 ON = 1~%uint8 ERROR = 2~%~%#Assigned control state~%uint8 ctrl_state~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'CtrlState)))
  "Returns full string definition for message of type 'CtrlState"
  (cl:format cl:nil "#CtrlState message~%~%#List of the available control states~%uint8 OFF = 0~%uint8 ON = 1~%uint8 ERROR = 2~%~%#Assigned control state~%uint8 ctrl_state~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <CtrlState>))
  (cl:+ 0
     1
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <CtrlState>))
  "Converts a ROS message object to a list"
  (cl:list 'CtrlState
    (cl:cons ':ctrl_state (ctrl_state msg))
))
