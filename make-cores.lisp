(require 'asdf)

;;make sure not to get any of my customized libraries,
;;only the ones in the system registry
(asdf:initialize-source-registry
 '(:source-registry
   (:tree "/usr/local/share/common-lisp/source")
   :ignore-inherited-configuration))

(asdf:initialize-output-translations
 '(:output-translations
   :ignore-inherited-configuration
   (T ("/var/cache/common-lisp" :implementation))))

(proclaim '(optimize (compilation-speed 0)))

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (sb-ext:invalid-fasl ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

(require 'sb-posix)

(defparameter +core-defs+
  '(("sbcl-swank.core" (swank))
    ;;include a bunch of the asdf-installed packages.
    ("sbcl-site.core" (asdf-system-connections cl-base64 cl-fad
    		       cl-interpol cl-ppcre flexi-streams
    		       iterate net-telent-date puri rfc2109
    		       trivial-gray-streams split-sequence sb-queue alexandria))
    ("sbcl-site-ucw-publish.core" (arnesi parenscript usocket
    				   closure-common cxml closure-html
    				   closer-mop puri ))
    ;;some others we track by source, now almost everything to run ucw.
    ("sbcl-site-ucw.core" (cffi clsql vecto metabang-bind cl-smtp))
    ))
(asdf:oos 'asdf:load-op :swank)
(funcall (symbol-function (find-symbol "INIT" :swank-loader)) :load-contribs t )

(loop while (> (length (sb-thread:list-all-threads)) 1)
      do (sleep 1))


(mapc
 #'(lambda (core-desc)
     (destructuring-bind (core-name sys-list) core-desc
       (dolist (s sys-list) (asdf:oos 'asdf:load-op s))
       ;;save a core of what we have so far,
       ;;and keep loading packages for the next core.
       (when (zerop (sb-posix:fork))
	 (asdf:clear-configuration)
	 (save-lisp-and-die core-name))))
 +core-defs+
 )

;;make sure all the children are done.
(sb-posix:wait)
(sb-ext:quit)
