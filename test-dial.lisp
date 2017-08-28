;;; Copyright 2017, Nicholas Patrick
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:test-clim-dial
  (:use #:clim #:clim-lisp #:clim-dial)
  (:export #:test-dial  #:test-clock))

(in-package :test-clim-dial)

(defun safe-read-number (string)
  "Read only from strings that contain the safe set of characters after trimming spaces."
  (if (some (lambda (char)
	      (not (member char '(#\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
	    (string-trim '(#\space #\tab) string))
      (error "String \"~A\" contains something other than a number" string)
      (the real (read-from-string string))))

(defun set-dial-value (value-gadget value)
  (declare (ignore value-gadget))
  ;(print "set-dial-value")
  (let ((dial1 (find-pane-named *application-frame* 'dial1))
	(dial2 (find-pane-named *application-frame* 'dial2))
	(new-value (handler-case (safe-read-number value)
		     (error nil))))
    (unless new-value
      (setf new-value 0))
    (when (and dial1 dial2)
      (setf (gadget-value dial1) new-value)
      (setf (gadget-value dial2) new-value))))

(define-application-frame dial-test ()
  ()
  (:panes (dial1 (make-pane 'dial-pane
			    :value 0.0
			    :min-value 0.0
			    :max-value 10.0
			    :radius 50
			    :height 55))
	  (dial2 (make-pane 'dial-pane
			    :value 0
			    :min-value -100
			    :max-value 100
			    :arc-start 0
			    :arc-start pi
			    :orientation :clockwise
			    :radius 75
			    ;:height 100
			    ))
	  (value
	   (outlining ()
	     (horizontally ()
	       (3/8 (make-pane 'label-pane
			       :label "Dial value"))
	       (5/8 (make-pane 'text-field-pane
			       :value "0"
			       :editable-p t
			       :value-changed-callback #'set-dial-value
			       :max-width 100))))))
  (:layouts (default (vertically ()
		       dial1
		       value
		       dial2))))

(defun test-dial (&optional make-thread)
  (let ((frame (make-application-frame 'dial-test)))
    (if make-thread
	(progn (bt:make-thread (lambda () (run-frame-top-level frame)))
	       frame)
	(run-frame-top-level frame))))
