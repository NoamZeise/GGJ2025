(in-package :ggj2025)

(defparameter *target-width* 800)
(defparameter *target-height* 450)
(defparameter *inverse-target-mat* (gficl:make-matrix))

(defun mouse-pos ()
  (destructuring-bind (x y) (gficl:mouse-pos :as-ndc)
    (let ((v (gficl:mat*vec *inverse-target-mat* (gficl:make-vec (list x y 0 1)))))
      (setf x (/ (+ 1 (gficl:vec-ref v 0)) 2))
      (setf y (/ (+ 1 (gficl:vec-ref v 1)) 2))
      (list (* x *target-width*) (* y *target-height*)))))
