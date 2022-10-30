;;; packages

(defpackage #:quicklisp-doctor
  (:documentation "Utility for checking quicklisp and local-projects")
  (:use :cl :esrap)
  (:export #:examine-declaration
           #:describe-workstation))
