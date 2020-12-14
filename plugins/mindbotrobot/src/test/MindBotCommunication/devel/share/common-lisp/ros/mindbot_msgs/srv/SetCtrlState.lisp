; Auto-generated. Do not edit!


(cl:in-package mindbot_msgs-srv)


;//! \htmlinclude SetCtrlState-request.msg.html

(cl:defclass <SetCtrlState-request> (roslisp-msg-protocol:ros-message)
  ((ctrl_state
    :reader ctrl_state
    :initarg :ctrl_state
    :type mindbot_msgs-msg:CtrlState
    :initform (cl:make-instance 'mindbot_msgs-msg:CtrlState)))
)

(cl:defclass SetCtrlState-request (<SetCtrlState-request>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <SetCtrlState-request>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'SetCtrlState-request)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-srv:<SetCtrlState-request> is deprecated: use mindbot_msgs-srv:SetCtrlState-request instead.")))

(cl:ensure-generic-function 'ctrl_state-val :lambda-list '(m))
(cl:defmethod ctrl_state-val ((m <SetCtrlState-request>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:ctrl_state-val is deprecated.  Use mindbot_msgs-srv:ctrl_state instead.")
  (ctrl_state m))
(cl:defmethod roslisp-msg-protocol:serialize ((msg <SetCtrlState-request>) ostream)
  "Serializes a message object of type '<SetCtrlState-request>"
  (roslisp-msg-protocol:serialize (cl:slot-value msg 'ctrl_state) ostream)
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <SetCtrlState-request>) istream)
  "Deserializes a message object of type '<SetCtrlState-request>"
  (roslisp-msg-protocol:deserialize (cl:slot-value msg 'ctrl_state) istream)
  msg
)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<SetCtrlState-request>)))
  "Returns string type for a service object of type '<SetCtrlState-request>"
  "mindbot_msgs/SetCtrlStateRequest")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetCtrlState-request)))
  "Returns string type for a service object of type 'SetCtrlState-request"
  "mindbot_msgs/SetCtrlStateRequest")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<SetCtrlState-request>)))
  "Returns md5sum for a message object of type '<SetCtrlState-request>"
  "60923bc5e5c8d30acc73ee0d6e56f3ab")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'SetCtrlState-request)))
  "Returns md5sum for a message object of type 'SetCtrlState-request"
  "60923bc5e5c8d30acc73ee0d6e56f3ab")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<SetCtrlState-request>)))
  "Returns full string definition for message of type '<SetCtrlState-request>"
  (cl:format cl:nil "#SetCtrlState service~%~%#REQUEST~%mindbot_msgs/CtrlState ctrl_state~%~%~%================================================================================~%MSG: mindbot_msgs/CtrlState~%#CtrlState message~%~%#List of the available control states~%uint8 OFF = 0~%uint8 ON = 1~%uint8 ERROR = 2~%~%#Assigned control state~%uint8 ctrl_state~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'SetCtrlState-request)))
  "Returns full string definition for message of type 'SetCtrlState-request"
  (cl:format cl:nil "#SetCtrlState service~%~%#REQUEST~%mindbot_msgs/CtrlState ctrl_state~%~%~%================================================================================~%MSG: mindbot_msgs/CtrlState~%#CtrlState message~%~%#List of the available control states~%uint8 OFF = 0~%uint8 ON = 1~%uint8 ERROR = 2~%~%#Assigned control state~%uint8 ctrl_state~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <SetCtrlState-request>))
  (cl:+ 0
     (roslisp-msg-protocol:serialization-length (cl:slot-value msg 'ctrl_state))
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <SetCtrlState-request>))
  "Converts a ROS message object to a list"
  (cl:list 'SetCtrlState-request
    (cl:cons ':ctrl_state (ctrl_state msg))
))
;//! \htmlinclude SetCtrlState-response.msg.html

(cl:defclass <SetCtrlState-response> (roslisp-msg-protocol:ros-message)
  ((success
    :reader success
    :initarg :success
    :type cl:boolean
    :initform cl:nil)
   (message
    :reader message
    :initarg :message
    :type cl:string
    :initform ""))
)

(cl:defclass SetCtrlState-response (<SetCtrlState-response>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <SetCtrlState-response>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'SetCtrlState-response)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-srv:<SetCtrlState-response> is deprecated: use mindbot_msgs-srv:SetCtrlState-response instead.")))

(cl:ensure-generic-function 'success-val :lambda-list '(m))
(cl:defmethod success-val ((m <SetCtrlState-response>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:success-val is deprecated.  Use mindbot_msgs-srv:success instead.")
  (success m))

(cl:ensure-generic-function 'message-val :lambda-list '(m))
(cl:defmethod message-val ((m <SetCtrlState-response>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:message-val is deprecated.  Use mindbot_msgs-srv:message instead.")
  (message m))
(cl:defmethod roslisp-msg-protocol:serialize ((msg <SetCtrlState-response>) ostream)
  "Serializes a message object of type '<SetCtrlState-response>"
  (cl:write-byte (cl:ldb (cl:byte 8 0) (cl:if (cl:slot-value msg 'success) 1 0)) ostream)
  (cl:let ((__ros_str_len (cl:length (cl:slot-value msg 'message))))
    (cl:write-byte (cl:ldb (cl:byte 8 0) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 8) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 16) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 24) __ros_str_len) ostream))
  (cl:map cl:nil #'(cl:lambda (c) (cl:write-byte (cl:char-code c) ostream)) (cl:slot-value msg 'message))
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <SetCtrlState-response>) istream)
  "Deserializes a message object of type '<SetCtrlState-response>"
    (cl:setf (cl:slot-value msg 'success) (cl:not (cl:zerop (cl:read-byte istream))))
    (cl:let ((__ros_str_len 0))
      (cl:setf (cl:ldb (cl:byte 8 0) __ros_str_len) (cl:read-byte istream))
      (cl:setf (cl:ldb (cl:byte 8 8) __ros_str_len) (cl:read-byte istream))
      (cl:setf (cl:ldb (cl:byte 8 16) __ros_str_len) (cl:read-byte istream))
      (cl:setf (cl:ldb (cl:byte 8 24) __ros_str_len) (cl:read-byte istream))
      (cl:setf (cl:slot-value msg 'message) (cl:make-string __ros_str_len))
      (cl:dotimes (__ros_str_idx __ros_str_len msg)
        (cl:setf (cl:char (cl:slot-value msg 'message) __ros_str_idx) (cl:code-char (cl:read-byte istream)))))
  msg
)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<SetCtrlState-response>)))
  "Returns string type for a service object of type '<SetCtrlState-response>"
  "mindbot_msgs/SetCtrlStateResponse")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetCtrlState-response)))
  "Returns string type for a service object of type 'SetCtrlState-response"
  "mindbot_msgs/SetCtrlStateResponse")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<SetCtrlState-response>)))
  "Returns md5sum for a message object of type '<SetCtrlState-response>"
  "60923bc5e5c8d30acc73ee0d6e56f3ab")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'SetCtrlState-response)))
  "Returns md5sum for a message object of type 'SetCtrlState-response"
  "60923bc5e5c8d30acc73ee0d6e56f3ab")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<SetCtrlState-response>)))
  "Returns full string definition for message of type '<SetCtrlState-response>"
  (cl:format cl:nil "~%#RESPONSE~%bool success~%string message~%~%~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'SetCtrlState-response)))
  "Returns full string definition for message of type 'SetCtrlState-response"
  (cl:format cl:nil "~%#RESPONSE~%bool success~%string message~%~%~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <SetCtrlState-response>))
  (cl:+ 0
     1
     4 (cl:length (cl:slot-value msg 'message))
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <SetCtrlState-response>))
  "Converts a ROS message object to a list"
  (cl:list 'SetCtrlState-response
    (cl:cons ':success (success msg))
    (cl:cons ':message (message msg))
))
(cl:defmethod roslisp-msg-protocol:service-request-type ((msg (cl:eql 'SetCtrlState)))
  'SetCtrlState-request)
(cl:defmethod roslisp-msg-protocol:service-response-type ((msg (cl:eql 'SetCtrlState)))
  'SetCtrlState-response)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetCtrlState)))
  "Returns string type for a service object of type '<SetCtrlState>"
  "mindbot_msgs/SetCtrlState")