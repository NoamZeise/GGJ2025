(in-package :ggj2025)

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
   (cam :initform (make-instance 'cam) :type cam)))

(defun make-game (width height)
  (let ((main-scene (make-main-scene))
	(player (make-player 0 0))
	(player-aim (make-game-obj 0 0 10 10 -0.1 (fw:get-asset 'arrow)))
	(col (make-game-obj 200 200 100 100 0 (fw:get-asset 'bouba))))
    (with-slots (hidden pivot) player-aim
      (setf hidden t)
      (setf pivot (gficl:make-vec '(5 0))))
    (fw:resize main-scene width height)
    (add-object main-scene
		(make-game-obj (- (/ *target-width* 2)) (- (/ *target-height* 2))
		     *target-width* *target-height*
		     -0.9 (fw:get-asset 'test)))
    (add-object main-scene player)
    (add-object main-scene col)
    (add-object main-scene player-aim)
    (make-instance
     'game
     :player player
     :main-scene main-scene
     :player-aim player-aim
     :colliders (list col))))

(defgeneric update-obj (obj dt))

(defmethod update-obj ((g game) dt)
  (with-slots (cam player player-aim main-scene colliders) g
    (with-slots (x y zoom) cam
      (let ((speed (* dt 150)))
	(gficl:map-keys-down
	 (:right (setf x (+ x speed)))
	 (:left (setf x (- x speed)))
	 (:up (setf y (- y speed)))
	 (:down (setf y (+ y speed)))
	 (:equal (setf zoom (+ zoom (* 0.4 dt))))
	 (:minus (setf zoom (- zoom (* 0.4 dt)))))))

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
    (set-view main-scene (view cam))
    (fw:update-scene (main-scene g) dt)))
