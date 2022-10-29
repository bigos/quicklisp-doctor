(declaim (optimize (speed 0) (safety 1) (debug 3)))

(in-package :quicklisp-doctor)

(defun run-program (program-and-args)
  (let ((process (sb-ext:run-program
                  (first program-and-args)
                  (rest  program-and-args)
                  :output :stream
                  :wait nil))
        (program-output (make-array '(0) :element-type 'character
                                         :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s program-output)
      (loop for line = (read-line (sb-ext:process-output process)
                                  nil nil)
            while line
            do (format s "~&~a" line))
      (sb-ext:process-close process)

      (list
        (sb-impl::process-exit-code process)
        program-output))))

;;; Am I going too far? Should we expect only one local projects folder?
(defun local-project-directories ()
  (alexandria:flatten
   (loop for lpd in ql:*local-project-directories*
         collect (uiop/filesystem::subdirectories lpd))))

(defun examine-folder (folder)
  (if (uiop/filesystem:directory-exists-p (merge-pathnames  folder ".git"))
      (list folder
            :git (first
                  (serapeum:split-sequence #\Newline
                                           (cadr
                                            (run-program (list "/usr/bin/git"
                                                               "-C" (namestring folder)
                                                               "log" "-1"))))))
      (list folder
            :no-git-detected)))


(defun print-relevant-info ()
  (format t "OS *************************~%")
  (format t "OS ~S~%" (uiop/os:operating-system))
  (format t "architecture ~S~%" (uiop/os:architecture))

  (format t "quicklisp *************************~%")
  (format t "home ~S~%" ql:*quicklisp-home*)
  (format t "client version ~S~%" (ql:client-version))
  (format t "latest dist version ~S~%" (caar (ql:available-dist-versions "quicklisp")))
  (format t "loaded dist version ~S~%" (ql:dist-version "quicklisp"))
  (format t "local projects ~S~%" ql:*local-project-directories*)

  (format t "git *************************~%")
  (format t "git path ~S~%" ; assumption on location of which, may cause problems on windows
          (run-program '("/usr/bin/which" "git")))
  (format t "git version ~S~%"
          (run-program '("/usr/bin/git" "--version")))

  (format t "git commits in local folders *************************~%")
  (loop for d in (local-project-directories)
        do
           (destructuring-bind (folder git-status &optional commit) (examine-folder d)
             (format t "~a ~a ~a~%" folder git-status (if commit commit "")))))

(defun examine-declaration (declaration-file)
  (warn "not finished ~S" declaration-file))
