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

(in-package #:clim-dial)

;;; "clim-dial" goes here. Hacks and glory await!

(defclass dial-pane (labelled-gadget-mixin
		     value-gadget
		     oriented-gadget-mixin
		     range-gadget-mixin
		     ;value-changed-repaint-mixin
		     )
  ((radius         :initform 25
		   :initarg :radius
		   :reader dial-radius)
   (arc-start      :initform 0
		   :initarg :arc-start
		   :reader dial-arc-start)
   (arc-end        :initform pi
		   :initarg :arc-end
		   :reader dial-arc-end))
  (:default-initargs :orientation :counter-clockwise)
  (:documentation "A slider gadget, except orientation is either :clockwise or :counter-clockwise."))
 
(defmethod compose-space ((pane dial-pane) &key width height)
  (declare (ignore width height))
  ;; Simple right now...require space for the full circle, sharpen the pencil later.
  (let ((diameter (* 2 (dial-radius pane))))
    (make-space-requirement :min-width  diameter :width  diameter
			    :min-height diameter :height diameter)))

(defun clamp (value min max)
  (cond ((< value min) min)
	((> value max) max)
	(t value)))

(defgeneric convert-value-to-angle (pane))
(defgeneric dial-arc-range (dial))

(defmethod dial-arc-range ((pane dial-pane))
  (- (dial-arc-end pane)
     (dial-arc-start pane)))

(defmethod convert-value-to-angle ((pane dial-pane))
  (let ((range (gadget-range pane)))
    ;; Percentage through range times angular range + arc-start
    (+ (dial-arc-start pane)
       (* (/ (- (gadget-value pane)
		(gadget-min-value pane))
	     range)
	  (dial-arc-range pane)))))

(defmethod handle-repaint ((pane dial-pane) region)
  (declare (ignore region))
  (let ((angle (convert-value-to-angle pane)))
    (multiple-value-bind (x1 y1 x2 y2)
	(bounding-rectangle* (sheet-region pane))
      (draw-rectangle* pane x1 y1 x2 y2
		       :filled t
		       :ink +background-ink+)
      (let* ((middle-x (round (- x2 x1) 2))
	     (middle-y (round (- y2 y1) 2))
	     (origin (make-point middle-x middle-y))
	     (rotation
	      (ecase (gadget-orientation pane)
		(:counter-clockwise
		 (make-rotation-transformation (- (dial-arc-start pane)
						  angle)
					       origin))
		(:clockwise
		 (make-rotation-transformation (+ (dial-arc-start pane)
						  angle)
					       origin)))))
	(draw-line pane
		   (make-point middle-x middle-y)
		   (make-point (+ middle-x (dial-radius pane)) middle-y)
		   :transformation rotation)))))

(defmethod (setf gadget-value) :around (new-value (gadget dial-pane)
					&key &allow-other-keys)
  (call-next-method (clamp new-value
			   (gadget-min-value gadget)
			   (gadget-max-value gadget))
		    gadget))

(defmethod (setf gadget-value) :after (new-value (gadget dial-pane)
                                       &key &allow-other-keys)
  (declare (ignore new-value))
  (queue-repaint gadget (make-instance 'window-repaint-event
				       :sheet gadget
				       :region (sheet-region gadget))))

