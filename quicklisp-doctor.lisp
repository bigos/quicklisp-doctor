(declaim (optimize (speed 0) (safety 1) (debug 3)))

(in-package :quicklisp-doctor)

;; (run-program "/usr/bin/hostname" (list))
;; (run-program "/usr/bin/which" (list "git"))
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

;;; copied from alexandria
(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

;;; Am I going too far? Should we expect only one local projects folder?
(defun local-project-directories ()
  (flatten
   (loop for lpd in ql:*local-project-directories*
         collect (uiop/filesystem::subdirectories lpd))))

(defun examine-folder (folder)
  (warn "examining folder ~S" folder)
  (warn "with directories ~S"
        (uiop/filesystem::subdirectories folder))


  (if (uiop/filesystem:directory-exists-p (merge-pathnames  folder ".git"))
      (list folder
            :git
            (run-program (list "/usr/bin/git" "-C" (namestring folder) "log" "-1")))
      (list folder
            :no-git-detected)))

(defun zzz ()
  (loop for d in (local-project-directories)
        collect (examine-folder d)))

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
  )

(defun examine-declaration (declaration-file)
  (warn "not finished ~S" declaration-file))
