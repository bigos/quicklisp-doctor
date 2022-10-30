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

;;; consider adding parsing of commands output at some point
(defun commit-info (commit)
  (let ((split (serapeum:split-sequence #\Newline commit)))
    (list (nth 0 split)
          (nth 2 split))))

(defun examine-folder (folder git-path)
  (if (uiop/filesystem:directory-exists-p (merge-pathnames  folder ".git"))
      (list folder
            :git (commit-info
                  (cadr
                   (run-program (list git-path
                                      "-C" (namestring folder)
                                      "log" "-1")))))
      (list folder
            :no-git-detected)))


(defun print-relevant-info ()
  (format t "OS *************************~%")
  (format t "OS ~S~%" (uiop/os:operating-system))
  (format t "architecture ~S~%" (uiop/os:architecture))

  (format t "quicklisp *************************~%")
  (format t "home ~S~%" ql:*quicklisp-home*)
  (format t "client version ~S~%" (ql:client-version))
  (let ((latest-quicklisp-version (caar (ql:available-dist-versions "quicklisp"))))
    (format t "latest dist version ~S~%" latest-quicklisp-version)
    (format t "loaded dist version ~S~%" (ql:dist-version "quicklisp"))
    (unless (equal latest-quicklisp-version (ql:dist-version "quicklisp"))
      (format t "~&~%!!!!!!!!!!!!!!!! WARNING your quicklisp is not up to date !!!!!!!!!!!!!!!!!!!!!!!!~%")
      (format t "In tour Lisp REPL please run ~&~a~%~a~%~%"
              "(ql:update-client)"
              "(ql:update-dist)")))
  
  (format t "local projects ~S~%" ql:*local-project-directories*)

  (format t "path *************************~%")
  (format t "depending onyour system Lisp and editor amy have different paths~%")
  (format t "current path ~A" (uiop:getenv "PATH"))


  (format t "git *************************~%")
  (format t "git path ~S~%" ; assumption on location of which, may cause problems on windows
          (run-program (list  (if (eq :win (uiop/os:operating-system))
                                  "c:/msys64/usr/bin/which.exe"
                                  "/usr/bin/which")
                              "git")))

  (let ((git-path (if (eq :win (uiop/os:operating-system))
                      "c:/msys64/usr/bin/git.exe"
                      "/usr/bin/git")))
    (warn "using git-path ~S" git-path)

    (format t "git version ~S~%"
            (run-program (list git-path "--version"))) ;and I may need c:/msys64 prefix fro my commands, aargh!

    (format t "git commits in local folders *************************~%")
    (loop for d in (local-project-directories)
          do
             (destructuring-bind (folder git-status &optional commit) (examine-folder d git-path)
               (format t "~a ~a ~a~%" folder git-status (if commit commit "")))))

  (defun examine-declaration (declaration-file)
    (warn "not finished ~S" declaration-file)))
