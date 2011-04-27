(in-package :asdf)

(defun file-stamp-universal-time (&optional (time (get-universal-time)))
  "returns a date as {y}{mon}{d}-{h}{min}{s}, defaults to get-universal-time
   intended for use in datestamping filenames
  "
  (multiple-value-bind ( s min h d mon y  )
      (decode-universal-time time)
    (format nil "~d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"  y mon d h min s)))

(defun log-with-room ( msg &rest args)
  (with-open-file (s "asdf-load-profile.log" :direction :output :if-exists :append)
    (let ((*debug-io* s)
	  (*trace-output* s)
	  (*standard-output* s))
      (write-sequence (file-stamp-universal-time) s)
      (write-sequence " - " s)
      (apply #'format s msg args)
      (room T))))

(defmethod perform :around ((operation asdf:load-op) component)
  (log-with-room "Loading ~S~%" component)
  (call-next-method)
  (log-with-room "DONE Loading ~S~%~%-------------~%~%" component))