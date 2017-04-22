(in-package :dial)

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
