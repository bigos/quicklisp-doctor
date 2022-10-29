(declaim (optimize (speed 0) (safety 1) (debug 3)))

(in-package :quicklisp-doctor)

;; (run-program "/usr/bin/hostname" (list))
;; (run-program "/usr/bin/which" (list "git"))
(defun run-program (program args)
  (let ((process (sb-ext:run-program
                  program
                  args
                  :output :stream :wait nil))
        (program-output (make-array '(0) :element-type 'base-char
                                    :fill-pointer 0 :adjustable t)))

    (with-output-to-string (s program-output)
      (loop for line = (read-line (sb-ext:process-output process)
                                  nil nil)
            while line
            do (format s "~a~%" line))
      (sb-ext:process-close process)

      (list :output program-output
            :exit-code (sb-impl::process-exit-code process)))))

(defun examine-declaration (declaration-file)
  (warn "not finished ~S" declaration-file))
