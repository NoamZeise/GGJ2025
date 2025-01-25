(in-package :ggj2025)

(defun make-2d-mat (x y w h &optional (depth 0) (rotation 0) (pivot (gficl:make-vec '(0 0))))
  (gficl:*mat
   (gficl:translation-matrix (list x y depth))
   (let ((px (gficl:vec-ref pivot 0))
	 (py (gficl:vec-ref pivot 1)))
     (if (not (= rotation 0))
	 (gficl:*mat
	  (gficl:translation-matrix (list px py 0))
	  (gficl:2d-rotation-matrix rotation)
	  (gficl:translation-matrix (list (- px) (- py) 0)))
       (gficl:make-matrix)))
   (gficl:scale-matrix (list w h 1))))

(defun contains (rect point)
  (destructuring-bind
   (x y w h) (gficl:vec-data rect)
   (destructuring-bind
    (px py) (gficl:vec-data point)
    (and (< x px)
	 (> x (- px w))
	 (< y py)
	 (> y (- py h))))))
