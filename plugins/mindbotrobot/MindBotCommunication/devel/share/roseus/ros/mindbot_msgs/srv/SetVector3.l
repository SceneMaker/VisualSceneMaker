;; Auto-generated. Do not edit!


(when (boundp 'mindbot_msgs::SetVector3)
  (if (not (find-package "MINDBOT_MSGS"))
    (make-package "MINDBOT_MSGS"))
  (shadow 'SetVector3 (find-package "MINDBOT_MSGS")))
(unless (find-package "MINDBOT_MSGS::SETVECTOR3")
  (make-package "MINDBOT_MSGS::SETVECTOR3"))
(unless (find-package "MINDBOT_MSGS::SETVECTOR3REQUEST")
  (make-package "MINDBOT_MSGS::SETVECTOR3REQUEST"))
(unless (find-package "MINDBOT_MSGS::SETVECTOR3RESPONSE")
  (make-package "MINDBOT_MSGS::SETVECTOR3RESPONSE"))

(in-package "ROS")

(if (not (find-package "GEOMETRY_MSGS"))
  (ros::roseus-add-msgs "geometry_msgs"))




(defclass mindbot_msgs::SetVector3Request
  :super ros::object
  :slots (_data ))

(defmethod mindbot_msgs::SetVector3Request
  (:init
   (&key
    ((:data __data) (instance geometry_msgs::Vector3 :init))
    )
   (send-super :init)
   (setq _data __data)
   self)
  (:data
   (&rest __data)
   (if (keywordp (car __data))
       (send* _data __data)
     (progn
       (if __data (setq _data (car __data)))
       _data)))
  (:serialization-length
   ()
   (+
    ;; geometry_msgs/Vector3 _data
    (send _data :serialization-length)
    ))
  (:serialize
   (&optional strm)
   (let ((s (if strm strm
              (make-string-output-stream (send self :serialization-length)))))
     ;; geometry_msgs/Vector3 _data
       (send _data :serialize s)
     ;;
     (if (null strm) (get-output-stream-string s))))
  (:deserialize
   (buf &optional (ptr- 0))
   ;; geometry_msgs/Vector3 _data
     (send _data :deserialize buf ptr-) (incf ptr- (send _data :serialization-length))
   ;;
   self)
  )

(defclass mindbot_msgs::SetVector3Response
  :super ros::object
  :slots (_success _message ))

(defmethod mindbot_msgs::SetVector3Response
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

(defclass mindbot_msgs::SetVector3
  :super ros::object
  :slots ())

(setf (get mindbot_msgs::SetVector3 :md5sum-) "efb349b09ac9e0d4c8ef08d9ad9508b4")
(setf (get mindbot_msgs::SetVector3 :datatype-) "mindbot_msgs/SetVector3")
(setf (get mindbot_msgs::SetVector3 :request) mindbot_msgs::SetVector3Request)
(setf (get mindbot_msgs::SetVector3 :response) mindbot_msgs::SetVector3Response)

(defmethod mindbot_msgs::SetVector3Request
  (:response () (instance mindbot_msgs::SetVector3Response :init)))

(setf (get mindbot_msgs::SetVector3Request :md5sum-) "efb349b09ac9e0d4c8ef08d9ad9508b4")
(setf (get mindbot_msgs::SetVector3Request :datatype-) "mindbot_msgs/SetVector3Request")
(setf (get mindbot_msgs::SetVector3Request :definition-)
      "#SetVector3 service

#REQUEST
geometry_msgs/Vector3 data


================================================================================
MSG: geometry_msgs/Vector3
# This represents a vector in free space. 
# It is only meant to represent a direction. Therefore, it does not
# make sense to apply a translation to it (e.g., when applying a 
# generic rigid transformation to a Vector3, tf2 will only apply the
# rotation). If you want your data to be translatable too, use the
# geometry_msgs/Point message instead.

float64 x
float64 y
---

#RESPONSE
bool success
string message

")

(setf (get mindbot_msgs::SetVector3Response :md5sum-) "efb349b09ac9e0d4c8ef08d9ad9508b4")
(setf (get mindbot_msgs::SetVector3Response :datatype-) "mindbot_msgs/SetVector3Response")
(setf (get mindbot_msgs::SetVector3Response :definition-)
      "#SetVector3 service

#REQUEST
geometry_msgs/Vector3 data


================================================================================
MSG: geometry_msgs/Vector3
# This represents a vector in free space. 
# It is only meant to represent a direction. Therefore, it does not
# make sense to apply a translation to it (e.g., when applying a 
# generic rigid transformation to a Vector3, tf2 will only apply the
# rotation). If you want your data to be translatable too, use the
# geometry_msgs/Point message instead.

float64 x
float64 y
---

#RESPONSE
bool success
string message

")



(provide :mindbot_msgs/SetVector3 "efb349b09ac9e0d4c8ef08d9ad9508b4")


