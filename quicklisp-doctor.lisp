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
    (loop for s in split
          when (or  (alexandria:starts-with-subseq "commit " s)
                    (alexandria:starts-with-subseq "Date: " s))
            collect s)))

(defun commit-commit (commit)
  (let ((split (serapeum:split-sequence #\Newline commit)))
    (loop for s in split
          when (or  (alexandria:starts-with-subseq "commit " s))
            collect s)))


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

(defun last-path-element (d)
  (car
   (last
    (butlast
     (serapeum:split-sequence #\/ (namestring d))))))

(defun describe-workstation ()
  (format t "OS *************************~%")
  (format t "OS ~S~%" (uiop/os:operating-system))
  (format t "architecture ~S~%" (uiop/os:architecture))
  (format t "Lisp ~S ~S~%" (lisp-implementation-type)
          (lisp-implementation-version))

  (format t "quicklisp *************************~%")
  (format t "home ~S~%" ql:*quicklisp-home*)
  (format t "client version ~S~%" (ql:client-version))
  (format t "enabled dists ~S~%"
          (loop for d in (ql-dist:enabled-dists) collect d))
  (let ((latest-quicklisp-version (caar (ql:available-dist-versions "quicklisp"))))
    (format t "latest dist version ~S~%" latest-quicklisp-version)
    (format t "loaded dist version ~S~%" (ql:dist-version "quicklisp"))
    (if (equal latest-quicklisp-version (ql:dist-version "quicklisp"))
        (format t "You are using the latest quicklisp.~%")
        (progn
          (format t "~&~%!!!!!!!!!!!!!!!! WARNING your quicklisp is not up to date !!!!!!!!!!!!!!!!!!!!!!!!~%")
          (format t "In your Lisp REPL please run ~&~a~%~a~%~%"
                  "(ql:update-client)"
                  "(ql:update-dist \"quicklisp\")"))))

  (format t "local projects ~S~%" ql:*local-project-directories*)

  (format t "paths *************************~%")
  (format t "Your Lisp and editor may have different paths, we may need that information.~%")
  (format t "path ~A~%" (uiop:getenv "PATH"))


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

    (format t "local-projects folders and their git info *************************~%")
    (loop for d in (local-project-directories)
          do
             (destructuring-bind (folder git-status &optional commit) (examine-folder d git-path)
               (format t "~a ~a ~a~%" folder git-status (if commit commit ""))))))

(defun workstation-attributes ()
  (let ((latest-quicklisp-version (caar (ql:available-dist-versions "quicklisp")))
        (git-path (if (eq :win (uiop/os:operating-system))
                      "c:/msys64/usr/bin/git.exe" ; works with MSYS2 on my system
                      "/usr/bin/git")))
    (list
     :system (list :os (uiop/os:operating-system)
                   :architecture (uiop/os:architecture)
                   :lisp-implementation-type (lisp-implementation-type)
                   :lisp-implementation-version (lisp-implementation-version))
     :quicklisp (list
                 :client-version (ql:client-version)
                 :enabled-dists (loop for d in (ql-dist:enabled-dists) collect d)
                 :dist-version (ql:dist-version "quicklisp")
                 :latest-version latest-quicklisp-version
                 (if (equal latest-quicklisp-version (ql:dist-version "quicklisp"))
                     :up-to-date
                     :needs-updating)
                 T
                 :local-projects ql:*local-project-directories*)
     :paths (uiop:getenv "PATH")
     :git (list :tried-path git-path
                :version (run-program (list git-path "--version")))
     :local-projects (loop for d in (local-project-directories)
                           collect (last-path-element d)
                           collect   (rest
                                      (examine-folder d git-path))))))

;; (quicklisp-doctor:list-git-local-projects "/usr/bin/git")
(defun list-git-local-projects (git-path)
  (loop for folder in (local-project-directories)
        when (uiop/filesystem:directory-exists-p (merge-pathnames  folder ".git"))
          collect
          (list
           :name (last-path-element folder)
           :commit (car (commit-commit (cadr  (run-program (list git-path
                                                                 "-C" (namestring folder)
                                                                 "log"
                                                                 "-1")))))
           :remote (cadr (run-program (list git-path
                                            "-C" (namestring folder)
                                            "remote"
                                            "get-url"
                                            "origin"))))))

(defun examine-commits (expected-name expected-commit available-commit git-path)
  "Examine local-project with EXPECTED-NAME checking EXPECTED-COMMIT and
available AVAILABLE-COMMIT commits to provide further advice"
  (let ((possible-folders (loop for d in ql:*local-project-directories*
                                collect (merge-pathnames d expected-name ))))
    (loop for folder in possible-folders
          collect
          (list :expected
                (run-program (list git-path
                                   "-C" (namestring folder)
                                   "log"
                                   (cadr (serapeum:split-sequence #\Space
                                                                  expected-commit))
                                   "-1"))
                :available
                (run-program (list git-path
                                   "-C" (namestring folder)
                                   "log"
                                   (cadr (serapeum:split-sequence #\Space
                                                                  available-commit))
                                   "-1"))))))

(defun examine-local-projects (expectations)
  (let ((workstation-attributes (workstation-attributes)))
    (list
     :quicklisp
     (cadr
      (member :quicklisp workstation-attributes))
     :local-projects
     (loop for expectation in expectations
           for expected-name =   (getf expectation :name)
           for expected-commit = (getf expectation :commit)
           for expected-remote = (getf expectation :remote)

           for project-git = (cadr (member expected-name (getf workstation-attributes :local-projects)
                                           :test #'equal))
           for available-commit = (caadr project-git)
           for status =  (progn
                           ;; TODO add handling of partial commit tokens
                           (if  (and (eq  (car project-git)
                                          :git)
                                     (equal expected-commit available-commit))
                                :looks-ok
                                (if project-git
                                    (list :commits-do-not-match expected-name
                                          :may-need-syncing-commits
                                          (examine-commits expected-name
                                                           expected-commit
                                                           available-commit
                                                           (getf (getf workstation-attributes :git) :tried-path)))

                                    (list :not-found expected-name :perhaps-needs-cloning expected-remote))))
           collect (list expected-name
                         status)))))
