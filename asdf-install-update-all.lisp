
(in-package :common-lisp-user)
(require 'asdf-install)

(defvar *packages*
  (copy-list ;copying so i can destructively modify this literal.
   '(acl-compat alexandria asdf-system-connections babel
     bordeaux-threads chunga cl-base64 metatilities
     moptilities cl-containers cl-fad
     cl-interpol cl-json cl-ppcre cl-sasl cl-utilities
     cl-vectors cl-who cl-xmpp contextl date-calc
     defsystem-compatibility fare-matcher fare-utils
     flexi-streams htmlgen ieee-floats ironclad iterate kmrcl lift
     net-telent-date postmodern puri split-sequence
     s-xml trivial-features trivial-garbage trivial-gray-streams
     trivial-sockets trivial-utf-8 usocket )))

(setf *packages*
      (copy-list '( lift)))


(defun asdf-install::where ()
  (elt asdf-install:*locations* 0))

(defun do-upgrades ()
  (let ((*print-circle* nil))
    (loop for p = (pop *packages*)
       while p
       do (with-simple-restart (skip-package "Skip upgrading ~a" p)
	    (handler-bind
		((undefined-function
		  (lambda (c)
		    (when (eql 'asdf::split (slot-value c 'sb-kernel::name))
		      (let ((r (find-restart 'asdf-install::skip-gpg-check)))
			(when r (invoke-restart r)))))))
		(asdf-install:install p))))))

'(fare-utils ironclad lift)