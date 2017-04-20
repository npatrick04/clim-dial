(in-package :dial)

(define-application-frame dial-test ()
  ()
  (:panes (dial (make-pane 'dial-pane
			   :value 0
			   :min-value 0
			   :max-value 100))
	  (value (horizontally ()
		   (make-pane 'text-field-pane
			      :editable-p nil
			      :value "Dial value")
		   (make-pane 'text-field-pane
			      :value "0"
			      :editable-p t
			      :value-changed-callback
			      (lambda (value-gadget value)
				(declare (ignore value-gadget))
				(let ((dial (find-pane-named *application-frame* 'dial))
				      (new-value
				       (handler-case (parse-integer value)
					 (error nil))))
				  (when (and dial new-value)
				    (setf (gadget-value dial) new-value))))))))
  (:layouts (default (vertically ()
		       dial
		       value))))

(defun test-dial ()
  (let ((frame (make-application-frame 'dial-test)))
    (run-frame-top-level frame)))
