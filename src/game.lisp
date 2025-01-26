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

(defclass game ()
  ((player :initarg :player)
   (player-aim :initarg :player-aim)
   (colliders :initarg :colliders)
   (main-scene :initarg :main-scene :type scene :accessor main-scene)
   (bg-scene :initarg :bg-scene :type scene :accessor bg-scene)
   (cam :initform (make-instance 'cam) :type cam)))

(defun make-game (width height)
  (let ((main-scene (make-main-scene))
	(bg-scene (make-bg-scene))
	(player (make-player 0 0))
	(player-aim (make-game-obj 0 0 10 10 -0.1 (fw:get-asset 'arrow)))
	(col (make-game-obj 200 200 100 100 0 (fw:get-asset 'bouba))))
    ;; Bg Scene
    (fw:resize bg-scene width height)
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     0.5 (fw:get-asset 'bg-stars) 1.5 4))
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
			     -0.98 (fw:get-asset 'bg-clouds1) 0.07 0.2))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.985 (fw:get-asset 'bg-stars) 0.05 2))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.99 (fw:get-asset 'bg0) 0.05 0.2))
    
    ;; Main Scene
    (with-slots (hidden pivot) player-aim
      (setf hidden t)
      (setf pivot (gficl:make-vec '(5 0))))
    (fw:resize main-scene width height)
    (add-object main-scene player)
    (add-object main-scene col)
    (add-object main-scene player-aim)    
    (make-instance
     'game
     :player player
     :main-scene main-scene
     :bg-scene bg-scene
     :player-aim player-aim
     :colliders (list col))))

(defgeneric update-obj (obj dt))

(defmethod update-obj ((g game) dt)
  (with-slots (cam player player-aim colliders) g
    (with-slots (x y zoom) cam)
    (let ((physics-dt (* dt *game-speed*)))
      (update-obj player physics-dt)
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
	(setf (slot-value player-aim 'hidden) t))
      (loop for c in colliders do
	    (progn
	      (update-obj c physics-dt)
	      (collision player c))))
    
    (target cam (get-target player) (selected player) dt)
    (update-cam-view cam)
    (with-slots (main-scene bg-scene) g
      (set-view main-scene (view cam))
      (set-view bg-scene (view cam)))
    (fw:update-scene (main-scene g) dt)
    (fw:update-scene (bg-scene g) dt)))
