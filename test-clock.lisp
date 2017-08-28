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

(in-package :test-clim-dial)

(define-application-frame clock-test ()
  ((done :accessor clock-test-done
	 :initform nil)
   (updater :accessor clock-test-updater))
  (:panes (clock (make-pane 'clock-pane
			    :name 'clock
			    :diameter 200))
	  (digital (make-pane 'text-field
			      :editable nil
			      :max-width 200)))
  (:layouts (default
		(vertically ()
		  clock
		  digital))))

;;; This is what creates the update tick for the clock.
(defmethod initialize-instance :after ((frame clock-test) &key &allow-other-keys)
  (setf (clock-test-updater frame)
	(bt:make-thread
	 (lambda ()
	   ;; TODO: use a semaphore to note when it's ready
	   (sleep 0.5)
	   (let ((clock (find-pane-named frame 'clock))
		 (digital (find-pane-named frame 'digital))
		 last-time)
	     (loop
		(when (clock-test-done frame)
		  (return nil))
		(let ((time (multiple-value-list
			     (get-decoded-time))))
		  ;; Only update the clock face when the time ticks
		  (when (not (equalp time last-time))
		    (setf last-time time)
		    (setf (gadget-value clock) time)
		    (setf (gadget-value digital)
			  (format nil "~{~2,'0D:~2,'0D:~2,'0D~}"
				  (reverse
				   (subseq time 0 3))))
		    (queue-repaint clock (make-instance 'window-repaint-event
							:sheet clock
							:region (sheet-region clock)))))
		;; This sleep is short so that it is responsive to the quit command.
		(sleep 0.1)))))))

(defmethod frame-exit :before ((frame clock-test))
  (setf (clock-test-done frame) t)
  (when (and (clock-test-updater frame)
	     (bt:threadp (clock-test-updater frame))
	     (bt:thread-alive-p (clock-test-updater frame)))
    (bt:join-thread (clock-test-updater frame))))

(defun test-clock (&optional make-thread)
  "Run the clock app, when MAKE-THREAD is NIL, run in the current thread, 
otherwise run it in a sub-thread, returning the frame."
  (let ((frame (make-application-frame 'clock-test)))
    (if make-thread
	(progn (bt:make-thread (lambda () (run-frame-top-level frame)))
	       frame)
	(run-frame-top-level frame))))
