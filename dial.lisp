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

;;; The abstract dial gadget is a subclass of slider (CLIM Spec #30.4.5)

(defclass dial (slider)
  ((radius         :initform 25
		   :initarg :radius
		   :reader dial-radius)
   (arc-start      :initform 0
		   :initarg :arc-start
		   :reader dial-arc-start)
   (arc-end        :initform pi
		   :initarg :arc-end
		   :reader dial-arc-end))
  (:default-initargs
   :orientation :counter-clockwise
    ;:background (compose-in +white+ +transparent-ink+)
   )
  (:documentation "A slider gadget, except orientation is either :clockwise or :counter-clockwise."))

(defconstant +2pi+ (* 2 pi))

(defun point-of-angle (angle)
  "Not really a point, but a list of the x/y of an angle."
  (list (cos angle) (sin angle)))

(defun inside-arc? (start angle end)
  "Is an ANGLE inside a counter-clockwise arc from START to END?"
  (let ((span  (mod (- end start) +2pi+))
	(dist (mod (- angle start) +2pi+)))
    (<= dist span)))

(defun bounding-rectangle-from-angles (&rest angles)
  "Including 0,0, find the bounding rectangles including all angles (radians)."
  (let* ((center (list 0 0))
	 (points (cons center
		       (mapcar #'point-of-angle angles))))
    (make-bounding-rectangle
     (reduce #'min points :key #'car)
     (reduce #'min points :key #'cadr)
     (reduce #'max points :key #'car)
     (reduce #'max points :key #'cadr))))

(defun arc-bounding-box (start end orientation)
  "Returns the unit-circle bounding rectangle of a dial arc
from start to end with orientation.  Angles are in radians.
Orientation is either :CLOCKWISE or :COUNTER-CLOCKWISE."
  (declare (type (member :clockwise :counter-clockwise) orientation)
	   (optimize debug))
  (let ((length (abs (- end start)))
	(most-ccw (ecase orientation
		    (:clockwise start)
		    (:counter-clockwise end)))
	(most-cw (case orientation	;the prior ecase covers us here.
		   (:clockwise end)
		   (:counter-clockwise start))))
    (if (>= length +2pi+)
	(make-bounding-rectangle -1 -1 1 1)
	(apply #'bounding-rectangle-from-angles
	       (remove-if-not (lambda (angle)
				(inside-arc? most-cw angle most-ccw))
			      (list 0
				    (/ pi 2)
				    pi
				    (* pi 3/2)
				    start
				    end))))))

(defun clamp (value min max)
  (cond ((< value min) min)
	((> value max) max)
	(t value)))

(defgeneric convert-value-to-angle (pane))
(defgeneric dial-arc-range (dial))

(defmethod dial-arc-range ((pane dial))
  (- (dial-arc-end pane)
     (dial-arc-start pane)))

;;; Return the delta from start angle
(defmethod convert-value-to-angle ((dial dial))
  (let* ((range (gadget-range dial))
	 (percent-through-range
	  (/ (- (gadget-value dial)
		(gadget-min-value dial))
	     range)))
    (* percent-through-range
       (dial-arc-range dial))))

(defun draw-dial (pane dial origin)
  "Given a PANE on which to draw DIAL, draw it from ORIGIN."
  (let ((angle (convert-value-to-angle dial)))
    (let* (
	   (rotation
	    (ecase (gadget-orientation dial)
	      (:counter-clockwise
	       (make-rotation-transformation (- (dial-arc-start dial)
						angle)
					     origin))
	      (:clockwise
	       (make-rotation-transformation (+ (dial-arc-start dial)
						angle)
					     origin)))))

      (draw-line pane
		 origin
		 (make-point (+ (point-x origin) (dial-radius dial))
			     (point-y origin))
		  :transformation rotation))))

;; TODO consider alignment
;; TODO consider number-of-quanta
(defmethod handle-repaint ((dial dial) region)
  (let* ((bb (arc-bounding-box (dial-arc-start dial)
			       (dial-arc-end dial)
			       (gadget-orientation dial)))
	 (origin-x (round (abs (* (dial-radius dial) (bounding-rectangle-min-x bb)))))
	 (origin-y (round (abs (* (dial-radius dial) (bounding-rectangle-max-y bb)))))
	 (origin (make-point origin-x origin-y)))
    (draw-dial dial dial origin)))

(defmethod (setf gadget-value) :around (new-value (gadget dial)
					&key &allow-other-keys)
  (call-next-method (clamp new-value
			   (gadget-min-value gadget)
			   (gadget-max-value gadget))
		    gadget))

;;; The concrete dial class
(defclass dial-pane (dial slider-pane)
  ()
  (:default-initargs :orientation :counter-clockwise)
  (:documentation "A slider gadget pane intended to be used standalone."))

(defmethod compose-space ((pane dial-pane) &key width height)
  (let* ((bb (arc-bounding-box (dial-arc-start pane)
			       (dial-arc-end pane)
			       (gadget-orientation pane)))
	 (bb-width  (* (dial-radius pane) (bounding-rectangle-width bb)))
	 (bb-height (* (dial-radius pane) (bounding-rectangle-height bb))))
    (make-space-requirement :min-width  bb-width  :width  (or width bb-width)
			    :min-height bb-height :height (or height bb-height))))

(defmethod handle-repaint :before ((pane dial-pane) region)
  ;(print "handle-repaint before dial-pane")
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* (sheet-region pane))
    (draw-rectangle* pane x1 y1 x2 y2
		     :filled t
		     :ink +background-ink+)))
