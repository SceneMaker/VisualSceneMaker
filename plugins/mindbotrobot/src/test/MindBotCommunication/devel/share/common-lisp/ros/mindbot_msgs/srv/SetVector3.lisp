; Auto-generated. Do not edit!


(cl:in-package mindbot_msgs-srv)


;//! \htmlinclude SetVector3-request.msg.html

(cl:defclass <SetVector3-request> (roslisp-msg-protocol:ros-message)
  ((data
    :reader data
    :initarg :data
    :type geometry_msgs-msg:Vector3
    :initform (cl:make-instance 'geometry_msgs-msg:Vector3)))
)

(cl:defclass SetVector3-request (<SetVector3-request>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <SetVector3-request>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'SetVector3-request)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-srv:<SetVector3-request> is deprecated: use mindbot_msgs-srv:SetVector3-request instead.")))

(cl:ensure-generic-function 'data-val :lambda-list '(m))
(cl:defmethod data-val ((m <SetVector3-request>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:data-val is deprecated.  Use mindbot_msgs-srv:data instead.")
  (data m))
(cl:defmethod roslisp-msg-protocol:serialize ((msg <SetVector3-request>) ostream)
  "Serializes a message object of type '<SetVector3-request>"
  (roslisp-msg-protocol:serialize (cl:slot-value msg 'data) ostream)
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <SetVector3-request>) istream)
  "Deserializes a message object of type '<SetVector3-request>"
  (roslisp-msg-protocol:deserialize (cl:slot-value msg 'data) istream)
  msg
)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<SetVector3-request>)))
  "Returns string type for a service object of type '<SetVector3-request>"
  "mindbot_msgs/SetVector3Request")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetVector3-request)))
  "Returns string type for a service object of type 'SetVector3-request"
  "mindbot_msgs/SetVector3Request")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<SetVector3-request>)))
  "Returns md5sum for a message object of type '<SetVector3-request>"
  "efb349b09ac9e0d4c8ef08d9ad9508b4")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'SetVector3-request)))
  "Returns md5sum for a message object of type 'SetVector3-request"
  "efb349b09ac9e0d4c8ef08d9ad9508b4")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<SetVector3-request>)))
  "Returns full string definition for message of type '<SetVector3-request>"
  (cl:format cl:nil "#SetVector3 service~%~%#REQUEST~%geometry_msgs/Vector3 data~%~%~%================================================================================~%MSG: geometry_msgs/Vector3~%# This represents a vector in free space. ~%# It is only meant to represent a direction. Therefore, it does not~%# make sense to apply a translation to it (e.g., when applying a ~%# generic rigid transformation to a Vector3, tf2 will only apply the~%# rotation). If you want your data to be translatable too, use the~%# geometry_msgs/Point message instead.~%~%float64 x~%float64 y~%float64 z~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'SetVector3-request)))
  "Returns full string definition for message of type 'SetVector3-request"
  (cl:format cl:nil "#SetVector3 service~%~%#REQUEST~%geometry_msgs/Vector3 data~%~%~%================================================================================~%MSG: geometry_msgs/Vector3~%# This represents a vector in free space. ~%# It is only meant to represent a direction. Therefore, it does not~%# make sense to apply a translation to it (e.g., when applying a ~%# generic rigid transformation to a Vector3, tf2 will only apply the~%# rotation). If you want your data to be translatable too, use the~%# geometry_msgs/Point message instead.~%~%float64 x~%float64 y~%float64 z~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <SetVector3-request>))
  (cl:+ 0
     (roslisp-msg-protocol:serialization-length (cl:slot-value msg 'data))
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <SetVector3-request>))
  "Converts a ROS message object to a list"
  (cl:list 'SetVector3-request
    (cl:cons ':data (data msg))
))
;//! \htmlinclude SetVector3-response.msg.html

(cl:defclass <SetVector3-response> (roslisp-msg-protocol:ros-message)
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

(cl:defclass SetVector3-response (<SetVector3-response>)
  ())

(cl:defmethod cl:initialize-instance :after ((m <SetVector3-response>) cl:&rest args)
  (cl:declare (cl:ignorable args))
  (cl:unless (cl:typep m 'SetVector3-response)
    (roslisp-msg-protocol:msg-deprecation-warning "using old message class name mindbot_msgs-srv:<SetVector3-response> is deprecated: use mindbot_msgs-srv:SetVector3-response instead.")))

(cl:ensure-generic-function 'success-val :lambda-list '(m))
(cl:defmethod success-val ((m <SetVector3-response>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:success-val is deprecated.  Use mindbot_msgs-srv:success instead.")
  (success m))

(cl:ensure-generic-function 'message-val :lambda-list '(m))
(cl:defmethod message-val ((m <SetVector3-response>))
  (roslisp-msg-protocol:msg-deprecation-warning "Using old-style slot reader mindbot_msgs-srv:message-val is deprecated.  Use mindbot_msgs-srv:message instead.")
  (message m))
(cl:defmethod roslisp-msg-protocol:serialize ((msg <SetVector3-response>) ostream)
  "Serializes a message object of type '<SetVector3-response>"
  (cl:write-byte (cl:ldb (cl:byte 8 0) (cl:if (cl:slot-value msg 'success) 1 0)) ostream)
  (cl:let ((__ros_str_len (cl:length (cl:slot-value msg 'message))))
    (cl:write-byte (cl:ldb (cl:byte 8 0) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 8) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 16) __ros_str_len) ostream)
    (cl:write-byte (cl:ldb (cl:byte 8 24) __ros_str_len) ostream))
  (cl:map cl:nil #'(cl:lambda (c) (cl:write-byte (cl:char-code c) ostream)) (cl:slot-value msg 'message))
)
(cl:defmethod roslisp-msg-protocol:deserialize ((msg <SetVector3-response>) istream)
  "Deserializes a message object of type '<SetVector3-response>"
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
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql '<SetVector3-response>)))
  "Returns string type for a service object of type '<SetVector3-response>"
  "mindbot_msgs/SetVector3Response")
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetVector3-response)))
  "Returns string type for a service object of type 'SetVector3-response"
  "mindbot_msgs/SetVector3Response")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql '<SetVector3-response>)))
  "Returns md5sum for a message object of type '<SetVector3-response>"
  "efb349b09ac9e0d4c8ef08d9ad9508b4")
(cl:defmethod roslisp-msg-protocol:md5sum ((type (cl:eql 'SetVector3-response)))
  "Returns md5sum for a message object of type 'SetVector3-response"
  "efb349b09ac9e0d4c8ef08d9ad9508b4")
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql '<SetVector3-response>)))
  "Returns full string definition for message of type '<SetVector3-response>"
  (cl:format cl:nil "~%#RESPONSE~%bool success~%string message~%~%~%~%"))
(cl:defmethod roslisp-msg-protocol:message-definition ((type (cl:eql 'SetVector3-response)))
  "Returns full string definition for message of type 'SetVector3-response"
  (cl:format cl:nil "~%#RESPONSE~%bool success~%string message~%~%~%~%"))
(cl:defmethod roslisp-msg-protocol:serialization-length ((msg <SetVector3-response>))
  (cl:+ 0
     1
     4 (cl:length (cl:slot-value msg 'message))
))
(cl:defmethod roslisp-msg-protocol:ros-message-to-list ((msg <SetVector3-response>))
  "Converts a ROS message object to a list"
  (cl:list 'SetVector3-response
    (cl:cons ':success (success msg))
    (cl:cons ':message (message msg))
))
(cl:defmethod roslisp-msg-protocol:service-request-type ((msg (cl:eql 'SetVector3)))
  'SetVector3-request)
(cl:defmethod roslisp-msg-protocol:service-response-type ((msg (cl:eql 'SetVector3)))
  'SetVector3-response)
(cl:defmethod roslisp-msg-protocol:ros-datatype ((msg (cl:eql 'SetVector3)))
  "Returns string type for a service object of type '<SetVector3>"
  "mindbot_msgs/SetVector3")