;;; this file is for writing declarations that will be  used to ensure our
;;; quicklisp installation and local projects are OK

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :quicklisp-doctor))

;;; example run
;;; (load "~/quicklisp/local-projects/quicklisp-doctor/check-local-projects.lisp")


#|

This is example of checking local projects, the commits are probably out of
date. Projects that use it should maintain the list of expected commits

(format t "~S~%"
        (quicklisp-doctor:examine-local-projects
         '((:name "quicklisp-doctor"
            :commit "commit 760fd82b458d39c1da19d449ae7eb7237ec58ab1"
            :remote "git@github.com:bigos/quicklisp-doctor.git ")
           (:name "cl-confidence"
            :commit "commit 14dc40a0fde822f4aab4fefc9b40ef5fdcbcd666"
            :remote "git@github.com:melusina-org/cl-confidence.git")
           (:name "dot-cons-tree"
            :commit "commit 529dc869e6e46535f4d5567658d750068e55bd5b"
            :remote "git@github.com:bigos/dot-cons-tree.git"))))

|#
