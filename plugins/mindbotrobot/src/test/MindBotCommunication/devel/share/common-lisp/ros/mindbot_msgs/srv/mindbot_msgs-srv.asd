
(cl:in-package :asdf)

(defsystem "mindbot_msgs-srv"
  :depends-on (:roslisp-msg-protocol :roslisp-utils :geometry_msgs-msg
               :mindbot_msgs-msg
               :sensor_msgs-msg
)
  :components ((:file "_package")
    (:file "SetCtrlMode" :depends-on ("_package_SetCtrlMode"))
    (:file "_package_SetCtrlMode" :depends-on ("_package"))
    (:file "SetCtrlState" :depends-on ("_package_SetCtrlState"))
    (:file "_package_SetCtrlState" :depends-on ("_package"))
    (:file "SetJointState" :depends-on ("_package_SetJointState"))
    (:file "_package_SetJointState" :depends-on ("_package"))
    (:file "SetPose" :depends-on ("_package_SetPose"))
    (:file "_package_SetPose" :depends-on ("_package"))
    (:file "SetVector3" :depends-on ("_package_SetVector3"))
    (:file "_package_SetVector3" :depends-on ("_package"))
  ))