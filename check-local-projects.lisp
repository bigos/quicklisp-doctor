;;; this file is for writing declarations that will be  used to ensure our
;;; quicklisp installation and local projects are OK

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :quicklisp-doctor))

;; (load "~/quicklisp/local-projects/quicklisp-doctor/check-local-projects.lisp")
(in-package :quicklisp-doctor)

(format t "~S~%"
        (examine-local-projects '(("cl-confidence" "commit 14dc40a0fde822f4aab4fefc9b40ef5fdcbcd666")
                                  ("dot-cons-tree" "commit 529dc869e6e46535f4d5567658d750068e55bd5b"))))
