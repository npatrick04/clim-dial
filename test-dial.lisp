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
  (:use #:clim #:clim-lisp #:clim-dial))

(in-package :test-clim-dial)

(define-application-frame dial-test ()
  ()
  (:panes (dial1 (make-pane 'dial-pane
			    :value 0
			    :min-value 0
			    :max-value 100))
	  (dial2 (make-pane 'dial-pane
			    :value 0
			    :min-value -100
			    :max-value 100
			    :arc-start 0
			    :arc-start pi
			    :orientation :clockwise))
	  (value
	   (outlining ()
	     (horizontally ()
	       (3/8 (make-pane 'label-pane
			       :label "Dial value"))
	       (5/8 (make-pane 'text-field-pane
			       :value "0"
			       :editable-p t
			       :value-changed-callback
			       (lambda (value-gadget value)
				 (declare (ignore value-gadget))
				 (let ((dial1 (find-pane-named *application-frame* 'dial1))
				       (dial2 (find-pane-named *application-frame* 'dial2))
				       (new-value
					(handler-case (parse-integer value :junk-allowed t)
					  (error nil))))
				   (unless new-value
				     (setf new-value 0))
				   (when (and dial1 dial2)
				     (setf (gadget-value dial1) new-value)
				     (setf (gadget-value dial2) new-value))))))))))
  (:layouts (default (vertically ()
		       dial1
		       value
		       dial2))))

(defun test-dial ()
  (let ((frame (make-application-frame 'dial-test)))
    (bt:make-thread (lambda () (run-frame-top-level frame)))
    frame))
