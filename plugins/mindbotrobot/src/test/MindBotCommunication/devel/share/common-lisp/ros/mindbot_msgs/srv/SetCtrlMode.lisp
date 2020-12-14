; Auto-generated. Do not edit!


(cl:in-package mindbot_msgs-srv)


;//! \htmlinclude SetCtrlMode-request.msg.html

(cl:defclass <SetCtrlMode-request> (roslisp-msg-protocol:ros-message)
  ((ctrl_mode
    :reader ctrl_mode
    :initarg :ctrl_mode
    :type mindbot_msgs-msg:CtrlMode
    :initform (cl:make-instance 'mindbot_msgs-msg:CtrlMode)))
)

(cl:defclass SetCtrlMode-request (<SetCtrlMode-request>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <SetCtrlMode-request>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'SetCtrlMode-request)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-srv:<SetCtrlMode-request> is deprecated: use mindbot_msgs-srv:SetCtrlMode-request instead.")))

(cl:ensure-generic-function 'ctrl_mode-val :lambda-list '(m))
(cl:defmethod ctrl_mode-val ((m <SetCtrlMode-request>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:ctrl_mode-val is deprecated.  Use mindbot_msgs-srv:ctrl_mode instead.")
  (ctrl_mode m))
(cl:defmethod roslisp-msg-protocol:serialize ((msg <SetCtrlMode-request>) ostream)
  "Serializes a message object of type '<SetCtrlMode-request>"
  (roslisp-msg-protocol:serialize (cl:slot-value msg 'ctrl_mode) ostream)
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <SetCtrlMode-request>) istream)
  "Deserializes a message object of type '<SetCtrlMode-request>"
  (roslisp-msg-protocol:deserialize (cl:slot-value msg 'ctrl_mode) istream)
  msg
)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<SetCtrlMode-request>)))
  "Returns string type for a service object of type '<SetCtrlMode-request>"
  "mindbot_msgs/SetCtrlModeRequest")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetCtrlMode-request)))
  "Returns string type for a service object of type 'SetCtrlMode-request"
  "mindbot_msgs/SetCtrlModeRequest")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<SetCtrlMode-request>)))
  "Returns md5sum for a message object of type '<SetCtrlMode-request>"
  "f628f1e6fadee58433a3f4d581cef831")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'SetCtrlMode-request)))
  "Returns md5sum for a message object of type 'SetCtrlMode-request"
  "f628f1e6fadee58433a3f4d581cef831")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<SetCtrlMode-request>)))
  "Returns full string definition for message of type '<SetCtrlMode-request>"
  (cl:format cl:nil "#SetCtrlMode service~%~%#REQUEST~%mindbot_msgs/CtrlMode ctrl_mode~%~%~%================================================================================~%MSG: mindbot_msgs/CtrlMode~%#CtrlMode message~%~%#List of the available control modes~%uint8 MODE0 = 0~%uint8 MODE1 = 1~%uint8 MODE2 = 2~%~%#Assigned control mode~%uint8 ctrl_mode~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'SetCtrlMode-request)))
  "Returns full string definition for message of type 'SetCtrlMode-request"
  (cl:format cl:nil "#SetCtrlMode service~%~%#REQUEST~%mindbot_msgs/CtrlMode ctrl_mode~%~%~%================================================================================~%MSG: mindbot_msgs/CtrlMode~%#CtrlMode message~%~%#List of the available control modes~%uint8 MODE0 = 0~%uint8 MODE1 = 1~%uint8 MODE2 = 2~%~%#Assigned control mode~%uint8 ctrl_mode~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <SetCtrlMode-request>))
  (cl:+ 0
     (roslisp-msg-protocol:serialization-length (cl:slot-value msg 'ctrl_mode))
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <SetCtrlMode-request>))
  "Converts a ROS message object to a list"
  (cl:list 'SetCtrlMode-request
    (cl:cons ':ctrl_mode (ctrl_mode msg))
))
;//! \htmlinclude SetCtrlMode-response.msg.html

(cl:defclass <SetCtrlMode-response> (roslisp-msg-protocol:ros-message)
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

(cl:defclass SetCtrlMode-response (<SetCtrlMode-response>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <SetCtrlMode-response>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'SetCtrlMode-response)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-srv:<SetCtrlMode-response> is deprecated: use mindbot_msgs-srv:SetCtrlMode-response instead.")))

(cl:ensure-generic-function 'success-val :lambda-list '(m))
(cl:defmethod success-val ((m <SetCtrlMode-response>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:success-val is deprecated.  Use mindbot_msgs-srv:success instead.")
  (success m))

(cl:ensure-generic-function 'message-val :lambda-list '(m))
(cl:defmethod message-val ((m <SetCtrlMode-response>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:message-val is deprecated.  Use mindbot_msgs-srv:message instead.")
  (message m))
(cl:defmethod roslisp-msg-protocol:serialize ((msg <SetCtrlMode-response>) ostream)
  "Serializes a message object of type '<SetCtrlMode-response>"
  (cl:write-byte (cl:ldb (cl:byte 8 0) (cl:if (cl:slot-value msg 'success) 1 0)) ostream)
  (cl:let ((__ros_str_len (cl:length (cl:slot-value msg 'message))))
    (cl:write-byte (cl:ldb (cl:byte 8 0) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 8) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 16) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 24) __ros_str_len) ostream))
  (cl:map cl:nil #'(cl:lambda (c) (cl:write-byte (cl:char-code c) ostream)) (cl:slot-value msg 'message))
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <SetCtrlMode-response>) istream)
  "Deserializes a message object of type '<SetCtrlMode-response>"
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
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<SetCtrlMode-response>)))
  "Returns string type for a service object of type '<SetCtrlMode-response>"
  "mindbot_msgs/SetCtrlModeResponse")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetCtrlMode-response)))
  "Returns string type for a service object of type 'SetCtrlMode-response"
  "mindbot_msgs/SetCtrlModeResponse")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<SetCtrlMode-response>)))
  "Returns md5sum for a message object of type '<SetCtrlMode-response>"
  "f628f1e6fadee58433a3f4d581cef831")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'SetCtrlMode-response)))
  "Returns md5sum for a message object of type 'SetCtrlMode-response"
  "f628f1e6fadee58433a3f4d581cef831")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<SetCtrlMode-response>)))
  "Returns full string definition for message of type '<SetCtrlMode-response>"
  (cl:format cl:nil "~%#RESPONSE~%bool success~%string message~%~%~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'SetCtrlMode-response)))
  "Returns full string definition for message of type 'SetCtrlMode-response"
  (cl:format cl:nil "~%#RESPONSE~%bool success~%string message~%~%~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <SetCtrlMode-response>))
  (cl:+ 0
     1
     4 (cl:length (cl:slot-value msg 'message))
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <SetCtrlMode-response>))
  "Converts a ROS message object to a list"
  (cl:list 'SetCtrlMode-response
    (cl:cons ':success (success msg))
    (cl:cons ':message (message msg))
))
(cl:defmethod roslisp-msg-protocol:service-request-type ((msg (cl:eql 'SetCtrlMode)))
  'SetCtrlMode-request)
(cl:defmethod roslisp-msg-protocol:service-response-type ((msg (cl:eql 'SetCtrlMode)))
  'SetCtrlMode-response)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetCtrlMode)))
  "Returns string type for a service object of type '<SetCtrlMode>"
  "mindbot_msgs/SetCtrlMode")