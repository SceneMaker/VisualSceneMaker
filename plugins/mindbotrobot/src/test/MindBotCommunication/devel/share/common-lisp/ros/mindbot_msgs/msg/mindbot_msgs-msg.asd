
(cl:in-package :asdf)

(defsystem "mindbot_msgs-msg"
  :depends-on (:roslisp-msg-protocol :roslisp-utils )
  :components ((:file "_package")
    (:file "CtrlMode" :depends-on ("_package_CtrlMode"))
    (:file "_package_CtrlMode" :depends-on ("_package"))
    (:file "CtrlState" :depends-on ("_package_CtrlState"))
    (:file "_package_CtrlState" :depends-on ("_package"))
  ))