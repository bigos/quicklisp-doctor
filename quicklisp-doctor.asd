(in-package "COMMON-LISP-USER")

(asdf/defsystem:defsystem #:quicklisp-doctor
  :description "Utility for detecting problems with quicklisp and local-projects"
  :author "Jacek Podkanski"
  :version "0.0.1"
  :serial T
  :depends-on (:esrap)
  :components ((:file "packages")
               (:file "quicklisp-doctor")))


;; (push #p "~/Programming/Lisp/quicklisp-doctor/" asdf:*central-registry*)
;; (ql:quickload 'quicklisp-doctor)
;; (in-package :quicklisp-doctor)
;; (examine-declaration "~/Programming/Lisp/quicklisp-doctor/check-local-projects.lisp")
