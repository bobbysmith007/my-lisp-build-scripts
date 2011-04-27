;;;; This is the lisp init file for when we are building cores for deployment
;;;; We are expecting to have asdf,asdf-binary-locations already loaded in the core
;;;;

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
     (#+sbcl sb-ext:invalid-fasl
      #+allegro excl::file-incompatible-fasl-error
      #+lispworks conditions:fasl-error
      #-(or sbcl allegro lispworks) error ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

;(defparameter +build-dir+ (make-pathname :directory (pathname-directory *load-truename*)))


(defun setup-source-registry (&optional build-dir)
  "Need the directory to the system we are building, since it is
normally in the same directory as the Rakefile, we can assume the
current-working-directory"
  (let* ((build-dir (or build-dir
			(pathname
			 (concatenate 'string (sb-posix:getcwd) "/"))))
	 (lib-dir (merge-pathnames "lib/" build-dir)))
    (asdf:clear-source-registry)
    (asdf:initialize-source-registry
     `(:source-registry
       (:directory ,build-dir)
       (:tree ,lib-dir)
       (:tree "/usr/local/share/common-lisp/source")
       :ignore-inherited-configuration))))

;;Allows a number of additional optimizations based on not
;;rendering as much information. more info at:
;; http://www.sbcl.org/manual/Debugger-Policy-Control.html#Debugger
(proclaim `(optimize (debug ,(if (sb-posix:getenv "LIVE_SERVER")
				 2
				 0))))


;;turn pretty print off because we don't print anything that needs it
;; and having this off speeds things up a good bit.
(setf *print-pretty* nil)
