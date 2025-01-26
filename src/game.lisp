(in-package :ggj2025)

(defparameter *x* 0.0)
(defparameter *y* 0.0)
(defparameter *zoom* 1.0)

(defclass cam ()
  ((x :initform 0.0 :type float)
   (y :initform 0.0 :type float)
   (zoom :initform 1.0 :type float)
   (view :accessor view :initform (gficl:make-matrix) :type gficl:matrix)
   (focus-amount :initform 0.0 :type float)))

(defgeneric update-cam-view (cam))

(defmethod update-cam-view ((cam cam))
  (with-slots (x y zoom view) cam
    (setf view
	  (let ((w (/ (* *target-width*) 2))
		(h (/ (* *target-height*) 2)))
	    (gficl:*mat	     
	     (gficl:translation-matrix (list w h 0))
	     (gficl:scale-matrix (list zoom zoom 1))
	     (gficl:translation-matrix (list (- w) (- h) 0))
	     (gficl:translation-matrix (list (+ (- x) w) (+ (- y) h) 0)))))
    (setf *x* x)
    (setf *y* y)
    (setf *zoom* zoom)
    (setf *inverse-main-view-mat* (gficl:inverse-matrix view))))

(defgeneric target (cam pos focus dt))

(defmethod target ((cam cam) pos focus dt)
  (with-slots (x y zoom focus-amount) cam
    (setf focus-amount
	  (+ focus-amount
	     (* (* dt (if focus 2 8))
		(- (if focus 1 0) focus-amount))))
    (setf x (gficl:vec-ref pos 0))
    (setf y (gficl:vec-ref pos 1))
    (setf zoom (+ 1 (* 0.8 focus-amount)))))

(defun get-cam-rect (cam)
  (with-slots (x y zoom) cam
    (gficl:make-vec (list (- x (/ (* zoom *target-width*) 2))
			  (- y (/ (* zoom *target-height*) 2))
			  (* zoom *target-width*) (* zoom *target-height*)))))

(defclass game ()
  ((player :initarg :player)
   (player-aim :initarg :player-aim)
   (colliders :initarg :colliders)
   (main-scene :initarg :main-scene :type scene :accessor main-scene)
   (bg-scene :initarg :bg-scene :type scene :accessor bg-scene)
   (cam :initform (make-instance 'cam) :type cam)
   (nearest-obs :initform 0)))

(defun make-game (width height)
  (let ((main-scene (make-main-scene))
	(player (make-player 0 0))
	(player-aim (make-game-obj 0 0 20 20 -0.1 (fw:get-asset 'arrow) 0))
	(dream (make-noise-obj 200 200 800 800 -0.1 0.01 (fw:get-asset 'dreamb) 0.2)))
    ;; Main Scene
    (with-slots (hidden pivot) player-aim
      (setf hidden t)
      (setf pivot (gficl:make-vec '(5 0))))
    (fw:resize main-scene width height)
    
    (add-object main-scene dream)
    (add-object main-scene player)
    (add-object main-scene player-aim)
    
    (make-instance
     'game
     :player player
     :main-scene main-scene
     :bg-scene (setup-bg width height)
     :player-aim player-aim
     :colliders (list dream))))

(defgeneric update-obj (obj dt))

(defmethod update-obj ((g game) dt)
  (with-slots (cam player player-aim colliders main-scene nearest-obs) g
    (with-slots (x y zoom) cam)
    (let ((physics-dt (* dt *game-speed*)))

      (gficl:map-keys-pressed
       (:p))

      (if (< (length colliders) 120)
	  (cond ((> nearest-obs (* 4 *target-width* *target-height*))
		 (let ((new-obs (gen-obstacles (get-cam-rect cam) 2)))
		   (loop for o in new-obs do (add-object main-scene o))
		   (setf colliders
			 (concatenate 'list new-obs colliders))))
		((< (length colliders) 10)
		 (let ((new-obs (gen-obstacles (get-cam-rect cam) 2)))
		   (loop for o in new-obs do (add-object main-scene o))
		   (setf colliders
			 (concatenate 'list new-obs colliders))))))
      (setf nearest-obs 0)
      
      (update-obj player physics-dt)
      
      (update-player-arrow player player-aim)
      (let* ((player-pos (get-game-object-pos player)))
	(setf colliders
	      (loop for c in colliders when (slot-value c 'active) collecting
		    (progn
		      (update-obj c physics-dt)
		      (collision player c)
		      (loop for c2 in colliders when (not (eql c c2)) do
			    (collision c c2))
		      (let* ((pos (get-game-object-pos c))
			     (diff (gficl:-vec player-pos pos))
			     (d (gficl:dot diff diff)))
			(if (or (> nearest-obs d) (= nearest-obs 0))
			    (setf nearest-obs d))
			(if (> d (* *target-width* *target-height* 10))
			    (setf (slot-value c 'active) nil)))
		      c)))))    
    (target cam (get-target player) (selected player) dt)
    (update-cam-view cam)
    (with-slots (bg-scene) g
      (set-view main-scene (view cam))
      (set-view bg-scene (view cam)))
    (fw:update-scene (main-scene g) dt)
    (fw:update-scene (bg-scene g) dt)))


;; Updates

(defun update-player-arrow (player player-aim)
  (if (selected player)
      (let* ((a (gficl:*vec 0.5 (aim-dir player)))
	     (size (gficl:magnitude a))
	     (angle
	      (+ (atan (gficl:vec-ref a 1) (gficl:vec-ref a 0))
		 (/ pi 2)))
	     (target (gficl:+vec a (get-target player))))
	(with-slots (rect rotation hidden pivot) player-aim
	  (setf hidden nil)
	  (setf rotation angle)
	  (setf (gficl:vec-ref rect 3) size)
	  (setf (gficl:vec-ref rect 0)
		(- (gficl:vec-ref target 0)
		   (* 0.5 (gficl:vec-ref rect 2))))
	  (setf (gficl:vec-ref rect 1)
		(- (gficl:vec-ref target 1)
		   (* 0 (gficl:vec-ref rect 3))))
	  (update-model player-aim)))
    (setf (slot-value player-aim 'hidden) t)))


;;; Setup


(defun setup-bg (width height)
  (let ((bg-scene (make-bg-scene)))
    (fw:resize bg-scene width height)
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     0.51 (fw:get-asset 'bg-stars) 1.5 4))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     0.5 (fw:get-asset 'bg-stars) 1.2 8))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.95 (fw:get-asset 'bg-stars) 0.1 0.8))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.96 (fw:get-asset 'bg1) 0.2 0.8))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.97 (fw:get-asset 'bg1) 0.09 1.5))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.981 (fw:get-asset 'bg-clouds1) 0.01 1))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.985 (fw:get-asset 'bg-stars) 0.05 2))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.99 (fw:get-asset 'bg0) 0.05 0.2))
    bg-scene))

(defun gen-obstacles (rect n)
  (loop for i from 0 to n collecting
	(let ((obs (get-obstacle))
	      (offset (random *target-width*)))
	  (with-slots ((r rect)) obs
	    (destructuring-bind
	     (ox oy ow oh) (gficl:vec-data r)
	     (declare (ignore ox oy))
	     (destructuring-bind
	      (x y w h) (gficl:vec-data rect)
	      (if (= 0 (random 2))
		  (progn
		    (setf (gficl:vec-ref r 0)
			  (+ (- x (+ ow 100)) (random (+ w ow 100))))
		    (setf (gficl:vec-ref r 1)
			  (if (= 0 (random 2))
			      (- y (+ (* 2 oh) offset)) (+ y h oh offset))))
		(progn
		  (setf (gficl:vec-ref r 0)
			(if (= 0 (random 2))
			    (- x (+ (* 2 ow) offset)) (+ x w ow offset)))
		  (setf (gficl:vec-ref r 1)
			(+ (- y (+ oh 100)) (random (+ h oh 100))))))
	      obs))))))

(defun get-obstacle ()
  (let* ((large (= 0 (random 5)))
	 (width (+ 80 (if large (random 400.0) (random 120.0))))
	 (objs (list
		`(make-game-obj 0 0 ,width ,width 0 (fw:get-asset 'gloogs) 0.5 :mass-mult 0.2)
		`(make-game-obj 0 0 ,width ,width 0 (fw:get-asset 'ringus) 0.5 :mass-mult 3)
		`(make-game-obj 0 0 ,width ,width 0 (fw:get-asset 'orring) 0.5 :mass-mult 1.2)
		`(make-game-obj 0 0 ,width ,width 0 (fw:get-asset 'bloog) 0.5 :mass-mult 0.8)))
	 (n (random (length objs))))
    (eval (nth n objs))))

(defun get-dream ()
  (let* ((objs (list
		`(make-noise-obj 0 0 800 800 -0.1 0.01 (fw:get-asset 'dreamb) 1))))))
