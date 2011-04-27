(in-package :cl-user)
(require 'asdf)

;; if we only want to use dependancies specified and
;; pulled with quicklisp
(defvar *quicklisp-only* nil)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun setup-source-registry ()
  (asdf:initialize-source-registry
   '(:source-registry
     (:tree "/usr/local/share/common-lisp/source")
     :ignore-inherited-configuration))
  (asdf:initialize-output-translations
   '(:output-translations
     :ignore-inherited-configuration
     (T (:user-cache :implementation)))))

(defun setup-quicklisp-source-registry ()
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(truename "~/quicklisp/"))
     (:tree ,(truename "~/local-projects/"))
     :ignore-inherited-configuration))
  (asdf:initialize-output-translations
   '(:output-translations
     :ignore-inherited-configuration
     (T (:user-cache :implementation)))))

(require 'split-sequence)

(defvar *systems-to-build*
  (list :adwcodebase :buildnode :talcl :css-selectors :ucw :ucw-buildnode))

(export '(run-build load-system test-system) :cl-user)

(defmacro with-load-system-error-handling (() &body body)
  `(let ((recompile-count 0)
	 (retry-count 0))
     (flet ((standard-error-handling (c)
	      (when (and (< recompile-count 5)
			 (find-restart 'asdf:try-recompiling))
		(incf recompile-count)
		(invoke-restart 'asdf:try-recompiling))
	      (when (and (< retry-count 5)
			 (find-restart 'asdf:retry))
		(incf retry-count)
		(invoke-restart 'asdf:retry))
	      (format *standard-output* "~S:~A~%~S~%~S~% after ~A recompile attempts and after ~A retry attempts~%~%"
		      (type-of c)
		      c c
		      (compute-restarts)
		      recompile-count retry-count)
	      (quit)))
       (handler-bind
	   ((asdf:missing-dependency
	     (lambda (c)
	       (when *quicklisp-only*
		 (format *standard-output*
			 "~%~%ATTEMPTING TO LOAD MISSING DEP ~S WITH QUICKLISP~%~%"
			 (asdf::missing-requires c))
		 (quicklisp:quickload (asdf::missing-requires c)))
	       (standard-error-handling c)))
	    (error #'standard-error-handling))
	 ,@body
	 ))))


(defun load-system (system)
  ;; to be run in the remote version
  (with-load-system-error-handling ()
    (asdf:oos 'asdf:load-op system)))

(defun test-system (system)
  ;; to be run in the remote version
  (load-system system)
  (with-load-system-error-handling ()
    (asdf:test-system system)))

(defun run-build (system &key (test t) output-file )
  (with-output-to-string (out)
    (sb-ext:run-program
     "sbcl" (list
	     "--script" "run-builds.lisp"
	     (if test
		 "--test-system"
		 "--load-system")
	     (princ-to-string system))
     :search t
     :output out)))

(defun process-arg (arg)
  (let ((last (- (length arg) 1)))
    (if (and (char-equal (elt arg 0) #\")
	     (char-equal (elt arg last) #\"))
	(subseq arg 1 last)
	arg
	)))

(defun process-args (args) (mapcar #'process-arg args))
(defun keywordize (s) (intern (string-upcase s) :keyword))
(defun read-system-list-from-client-string (s)
  (unless s (return-from read-system-list-from-client-string nil))
  (let ((last (- (length s) 1)))
    (when (and (char-equal (elt s 0) #\()
	       (char-equal (elt s last) #\)))
      (setf s (subseq s 1 last))))
  (remove-if
   #'null
   (mapcar
    #'keywordize
    (split-sequence:split-sequence
     #\space s
     :test #'char-equal :remove-empty-subseqs t))))


(defun main (&optional (args (process-args *posix-argv*)) )
  (let ((*quicklisp-only* (member "--quicklisp-only" args :test #'string-equal)))
    (sb-ext:run-program
     "delete-fasls.sh" (list (princ-to-string (truename "~/")))
     :search t)
    (if *quicklisp-only*
	(setup-quicklisp-source-registry)
	(setup-source-registry))
    (format *standard-output* "Running Build test ARGS:~S~%------------~%" *posix-argv*)
    (let ((test-systems
	   (read-system-list-from-client-string
	    (cadr (member "--test-system" args :test #'string-equal))))
	  (load-systems
	   (read-system-list-from-client-string
	    (cadr (member "--load-system" args  :test #'string-equal)))))
      (when load-systems
	(format *standard-output* "Running load-system ~S~%" load-systems)
	(mapcar #'load-system load-systems))
      (when test-systems
	(format *standard-output* "Running test-system ~S~%" test-systems)
	(mapcar #'test-system test-systems))
      )))

(funcall 'main)


