(defpackage :xrefer (:use :common-lisp :common-lisp-user :iterate :asdf))
(in-package :xrefer)
;;;;
;;;;  This is a standalone file to help do massive package/system wide
;;;;  cross references, to answere questions like which parts of
;;;;  package A am I using in package B
;;;;

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute),@body))

(eval-always
  (defun symbolize-string (str &optional (package *package*))
    "Turns a string into a happy symbol 
   ex: ''foo bar_bast'' -> FOO-BAR-BAST

   * can also 'change' package of symbol
   ex: :foo -> adw::foo
  "
    (intern
     (etypecase str
       (string (nsubstitute
		#\- #\_
		(nsubstitute #\- #\space (string-upcase str) :test #'char=)
		:test #'char=))
       (symbol (symbol-name str)))
     package))
  
  (defun ensure-list (it) (if (listp it) it (list it)))
  
  (defun slot-def (sym &optional initform)
    "given symbol, make a standard slot def for it
   indended for read-time-eval
   "
    `(,sym :accessor ,sym :initform ,initform :initarg ,(symbolize-string (princ-to-string sym) :keyword)))

  (defun slot-defs (syms)
    "handle a list of slotdefs
   indended for read-time-eval"
    (loop for args in (ensure-list syms)
	  collect (apply #'slot-def (ensure-list args))))
  
  (defun ensure-package-list (ps)
    (mapcar #'find-package (ensure-list ps))))


(defun file-stamp-universal-time (&optional (time (get-universal-time)))
  "returns a date as {y}{mon}{d}-{h}{min}{s}, defaults to get-universal-time
   intended for use in datestamping filenames
  "
  (multiple-value-bind ( s min h d mon y  )
      (decode-universal-time time)
    (format nil "~d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"  y mon d h min s)))

(defvar *xref-stream* T)

(defmacro with-log-out ( msg &rest args)
  (with-open-file (s "asdf-xref.log" :direction :output :if-exists :append)
    (let ((*debug-io* s)
	  (*standard-output* (make-broadcast-stream s *standard-output*))
	  (msg (format nil "~~a ~A ~~%" msg))
	  (args (cons (file-stamp-universal-time) args)))
      (apply #'format *standard-output* msg args))))

`(defun packages-for-system ()
  )

`(defun xref-system (system-name)
  (let* ((system (asdf:find-system system-name))
	 (depends-on (asdf::component-load-dependencies system))
	 (packages (packages-for-system system)))
    ))

(defclass xref () #.(slot-defs '(sym mentions)))
(defun add-mention (xref mention) (push mention (mentions xref)))

(defvar *current-xrefs* ())
(defvar *checked-symbols* ())

(defun mention-in-package? (mention package)
  (flet ((mention-name-symbol (mention)
	   (destructuring-bind (name location) mention
	     (declare (ignore location))
	     (cond
	       ((atom name) name)
	       ( ;; defmethod
		(and (listp name) (atom (second name)))
		(second name))
	       ( ;; setf defmethod
		(and (listp name) (listp (second name)))
		(second (second name)))
	       (T (error "What type of name is ~S" name)))
	     )))
    (member (symbol-package (mention-name-symbol mention))
      (ensure-package-list package))))

(defun do-xrefs ( sym package )
  (setf package (ensure-package-list package))
  (let* ((xref (make-instance 'xref :sym sym))
	 (mentioner (lambda (mention)
		      (when (mention-in-package? mention package)
			(add-mention xref mention)))))    
    (mapc mentioner
	  (append
	   (swank-backend:who-references sym)
	   (format *xref-stream* " refs" )
	   (swank-backend:who-calls sym)
	   (format *xref-stream* " calls" )
	   (swank-backend:who-macroexpands sym)
	   (format *xref-stream* " expands" )
	   (swank-backend:who-specializes sym)
	   (format *xref-stream* " specs" )
	   (swank-backend:who-sets sym)
	   (format *xref-stream* " sets" )))
    
    (when (and (mentions xref) (not (find-xref (sym xref))))
      (format *xref-stream* " storing")
      (push xref *current-xrefs*))
    (format *xref-stream* " done")
    xref))

(defvar *default-excludes*
  (ensure-package-list
   '(:common-lisp :common-lisp-user :iterate)))

(defun xref-package (&optional (package (find-package :buildnode))
		     (excludes *default-excludes*))
  (setf *current-xrefs* ()
	*checked-symbols* ()
	excludes (ensure-package-list excludes)
	package (ensure-package-list package))
  (let ((*package* (find-package :keyword)))
    (iter
      (for p in package)
      (iter
	(for used in (package-use-list p))
	(when (member used excludes) (next-iteration))
	(iter (for symbol in-package used)
	      (when (or (member (symbol-package symbol) excludes)
			(member symbol *checked-symbols*))
		(next-iteration))
	      (push symbol *checked-symbols*)
	      (format *xref-stream* "~S" symbol)
	      (do-xrefs symbol package)
	      (format *xref-stream* "~%")))))
  *current-xrefs*)


(defun find-xref (sym &optional (xrefs *current-xrefs*))
  (find sym xrefs :key #'sym ))

(defun format-xref-result (xref &optional verbose)
  (let ((*package* (find-package :keyword)))
    (format *xref-stream* "~S (~D): ~{~s, ~}"
	    (sym xref) (length (mentions xref)) (mapcar #'first (mentions xref)))
    (when verbose
      (format *xref-stream*
	      "~{-----------~%~s-----------~%~}" (mentions xref))
      )
    (format *xref-stream* "~%")))

(defun format-xrefs (&optional verbose (xrefs *current-xrefs*))
  (iter (for xref in xrefs)
	(format-xref-result xref verbose)))

