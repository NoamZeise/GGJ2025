(in-package :ggj2025)

(defparameter *target-width* 800)
(defparameter *target-height* 450)
(defparameter *inverse-target-mat* (gficl:make-matrix))
(defparameter *inverse-main-view-mat* (gficl:make-matrix))
(defparameter *game-speed* 1)

(defun mouse-pos (&optional view-corrected)
  (destructuring-bind
   (x y) (gficl:mouse-pos :as-ndc)
   (let ((v (gficl:mat*vec
	     *inverse-target-mat* (gficl:make-vec (list x y 0 1)))))
     (setf x (/ (+ 1 (gficl:vec-ref v 0)) 2))
     (setf y (/ (+ 1 (gficl:vec-ref v 1)) 2))
     (setf v (gficl:make-vec (list (* x *target-width*) (* y *target-height*) 0 1)))
     (if view-corrected
	 (setf v (gficl:mat*vec *inverse-main-view-mat* v)))
     (list (gficl:vec-ref v 0) (gficl:vec-ref v 1)))))
