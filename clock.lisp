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

(in-package :clim-dial)

(defclass clock-pane (value-gadget
		      immediate-repainting-mixin
		      sheet-multiple-child-mixin
		      sheet-with-medium-mixin
		      basic-pane)
  ((diameter :reader clock-pane-diameter
	     :initarg :diameter
	     :initform  50)
   ;; TODO: Implement a border
   (border :reader clock-pane-border
	   :initarg :border
	   :initform 1)
   (second-hand :reader clock-pane-second-hand
		:initarg :second-hand-p
		:initform t)
   (minute-hand :reader clock-pane-minute-hand
		:initarg :minute-hand-p
		:initform t)
   (hour-hand :reader clock-pane-hour-hand
	      :initarg :hour-hand-p
	      :initform t))
  (:documentation "The gadget value is the clock time"))

(defparameter *clock-second-spacing* 3)
(defparameter *clock-minute/second-ratio* 9/10)
(defparameter *clock-hour/second-ratio* 1/2)

(defmethod initialize-instance :after ((pane clock-pane) &key &allow-other-keys)
  (with-slots (diameter border second-hand minute-hand hour-hand) pane
    (let* ((second-radius (- (/ diameter 2) *clock-second-spacing*))
	   (minute-radius (round (* second-radius *clock-minute/second-ratio*)))
	   (hour-radius   (round (* second-radius *clock-hour/second-ratio*))))
      (multiple-value-bind (second minute hour &rest rest)
	  (get-decoded-time)
	(declare (ignore rest))
	(when second-hand
	  (setf second-hand
		(make-pane 'dial
			       :name 'second-hand
			       :value second
			       :min-value 0
			       :max-value 60
			       :arc-start (* pi -1/2)
			       :arc-end (* pi 3/2)
			       :number-of-quanta 60
			       :orientation :clockwise
			       :radius second-radius)))
	(when minute-hand
	  (setf minute-hand
		(make-pane 'dial
			       :name 'minute-hand
			       :value minute
			       :min-value 0
			       :max-value 60
			       :arc-start (* pi -1/2)
			       :arc-end (* pi 3/2)
			       :number-of-quanta (* 60 60)
			       :orientation :clockwise
			       :radius minute-radius)))
	(when hour-hand
	  (setf hour-hand
		(make-pane 'dial
			       :name 'hour-hand
			       :value hour
			       :min-value 0
			       :max-value 12
			       :arc-start (* pi -1/2)
			       :arc-end (* pi 3/2)
			       :number-of-quanta (* 60 60 12)
			       :orientation :clockwise
			       :radius hour-radius)))))))

(defmethod (setf gadget-value) (value (pane clock-pane)
				&key &allow-other-keys)
  (destructuring-bind (second minute hour &rest rest) value
    (declare (ignore rest))
    (when (clock-pane-second-hand pane)
      (setf (gadget-value (clock-pane-second-hand pane)) second))
    (when (clock-pane-minute-hand pane)
      (setf (gadget-value (clock-pane-minute-hand pane)) minute))
    (when (clock-pane-hour-hand pane)
      (setf (gadget-value (clock-pane-hour-hand pane)) (mod hour 12)))))
 
;;; Per 29.3.4 layout protocol, the clock can cover for the children panes
(defmethod compose-space ((pane clock-pane) &key width height)
  ;; Don't need to call compose-space on the children, this method covers them.
  (make-space-requirement :min-width  (clock-pane-diameter pane)
			  :width  (or width (clock-pane-diameter pane))
			  :min-height (clock-pane-diameter pane)
			  :height (or height (clock-pane-diameter pane))))

(defun clear-pane (pane)
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* (sheet-region pane))
    (draw-rectangle* pane x1 y1 x2 y2
		     :filled t
		     :ink +background-ink+)))

(defmethod handle-repaint :before ((pane clock-pane) region)
  (clear-pane pane))

(defmethod handle-repaint ((pane clock-pane) region)
  (let* ((radius (/ (clock-pane-diameter pane) 2))
	 (origin (make-point radius radius)))
    (with-slots (second-hand minute-hand hour-hand diameter) pane
      (when second-hand
	(draw-dial pane second-hand origin))
      (when minute-hand
	(draw-dial pane minute-hand origin))
      (when hour-hand
	(draw-dial pane hour-hand origin)))))
