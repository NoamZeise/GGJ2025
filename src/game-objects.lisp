(in-package :ggj2025)

(defclass collider ()
  ((pos :initarg :pos :initform (gficl:make-vec '(0 0)) :type gficl:vec)))

(defclass circle-collider (collider)
  ((radius :initarg :radius :initform 0.0 :type float)))

(defun make-circle-collider (pos radius)
  (make-instance 'circle-collider :pos pos :radius radius))

(defclass game-object (object)
  ((rect :initform (gficl:make-vec '(0 0 0 0)) :type gficl:vec)
   (velocity :initform (gficl:make-vec '(0 0)))
   (friction :initform 0.2 :type float)
   (pivot :initform (gficl:make-vec '(0 0)) :type gficl:vec)
   (rotation :initform 0.0 :initarg :rot :type float)
   (angular-velocity :initform 0.0 :type float)
   (angular-friction :initform 0.2 :type float)
   (depth :initform 0.0 :type float)
   (mass :initform 1.0 :type float)
   (collider :accessor collider :initform (make-instance 'circle-collider) :type collider)))

(defgeneric update-model (game-object))

(defmethod update-model ((obj game-object))
  (with-slots (rect depth rotation object pivot) obj
    (destructuring-bind
     (x y width height) (gficl:vec-data rect)
     (update-object-model obj (make-2d-mat x y width height depth rotation pivot)))))

(defgeneric get-target (obj))

(defmethod get-target ((obj game-object))
  (with-slots (rect) obj
    (destructuring-bind
     (x y w h) (gficl:vec-data rect)
     (gficl:make-vec (list (+ x (/ w 2)) (+ y (/ h 2)))))))

(defun get-game-object-pos (obj)
  (with-slots (rect) obj
    (gficl:get-n-vec 2 rect)))

(defun make-game-obj (x y width height depth tex-asset radius
			&key (mass-mult 1.0) (type 'game-object))
  (let ((obj (make-object (fw:get-asset 'quad) (gficl:make-matrix) tex-asset
			  type)))
    (with-slots (rect collider mass pivot (d depth)) obj
      (setf rect (gficl:make-vec (list x y width height)))
      (setf collider
	    (make-circle-collider (get-target obj)
				  (* radius (/ (max width height) 2))))
      (setf mass (* mass-mult (max width height)))
      (setf pivot (gficl:make-vec (list (/ width 2) (/ height 2))))
      (setf d depth))
    (update-model obj)
    obj))

(defmethod update-obj ((obj game-object) dt)
  (with-slots
      (rect velocity friction
       rotation angular-velocity angular-friction
       collider)
      obj
    (setf velocity
	  (gficl:-vec
	   velocity
	   (gficl:*vec (* dt friction) velocity)))
    (setf rect (gficl:+vec rect (gficl:*vec dt velocity)))
    (setf angular-velocity (- angular-velocity (* dt angular-friction angular-velocity)))
    (setf rotation (+ rotation (* dt angular-velocity)))
    (with-slots (pos) collider
      (setf pos (get-target obj)))
    (update-model obj)))

(defgeneric collision (obj1 obj2))

(defmethod collision ((c1 circle-collider) (c2 circle-collider))
  (with-slots ((p1 pos) (r1 radius)) c1
    (with-slots ((p2 pos) (r2 radius)) c2
      (let* ((v (gficl:-vec p1 p2))
	     (d (gficl:magnitude v))
	     (p (- d (+ r1 r2))))
	(if (>= p 0) nil
	  (list v d))))))

(defmethod collision ((obj1 game-object) (obj2 game-object))
  (with-slots ((c1 collider) (v1 velocity) (m1 mass) (a1 angular-velocity)) obj1
    (with-slots ((c2 collider) (v2 velocity) (m2 mass) (a2 angular-velocity)) obj2
      (let ((col (collision c1 c2)))
	(cond (col
	       (destructuring-bind
		(v d) col
		(with-slots ((x1 pos)) c1
		  (with-slots ((x2 pos)) c2
		    (let ((x1-x2 (gficl:-vec x1 x2))
			  (x2-x1 (gficl:-vec x2 x1))
			  (v1-v2 (gficl:-vec v1 v2))
			  (v2-v1 (gficl:-vec v2 v1)))
		      (setf v1
			    (gficl:-vec
			     v1
			     (gficl:*vec (* (/ (* 2 m2) (+ m1 m2))
					    (/ (gficl:dot v1-v2 x1-x2) (* d d)))
					 x1-x2)))
		      (setf v2
			    (gficl:-vec
			     v2
			     (gficl:*vec (* (/ (* 2 m1) (+ m1 m2))
					    (/ (gficl:dot v2-v1 x2-x1) (* d d)))
					 x2-x1)))
		      (setf a1 (/ (+ a1 (- a2)) 2))
		      (setf a2 (/ (+ a2 (- a1)) 2))))))))
	col))))

;;; Noise

(defclass noise-object (game-object)
  ((noise-speed :initform 0.0 :type float)))

(defun make-noise-obj (x y width height depth radius tex-asset mass-mult
			 &key (type 'noise-object))
  (let ((obj (make-game-obj x y width height depth tex-asset radius
			    :mass-mult mass-mult :type type)))
    (with-slots ((ns noise-speed) angular-friction angular-velocity) obj
      (setf ns (+ 0.01 (- (random 0.02) 0.01)))
      (setf angular-friction 0.0)
      (setf angular-velocity (/ (random 100) 400)))
    obj))

(defclass dream-goal (game-object) ())

(defclass bubble (dream-goal)
  ((following :initform nil :accessor following)))

(defmethod update-obj ((p bubble) dt)
	   (with-slots (following rect velocity angular-velocity selected colour aim-dir) p
	     (if following
		 (progn
		   (setf velocity
			 (gficl:*vec 3 (gficl:-vec following (gficl:get-n-vec 2 rect))))))
	     (call-next-method)))

(defclass dream-object (noise-object) ())

;;; Player

(defclass player (game-object)
  ((selected :initform nil :accessor selected)
   (aim-dir :initform (gficl:make-vec '(0 0)) :type gficl:vec :accessor aim-dir)
   (in-dream :initform nil)))

(defun make-player (x y)
  (let ((player (make-game-obj x y 80 80 -0.01 (fw:get-asset 'fairy) 0.64 :type 'player)))
    player))

(defmethod update-obj ((p player) dt)
  (with-slots (rect velocity angular-velocity selected colour aim-dir) p
    (destructuring-bind
     (mx my) (mouse-pos :view-corrected)
     (if selected
	 (let ((hit-vec (gficl:-vec
			 (get-target p)
			 (gficl:make-vec (list mx my)))))
	   (setf aim-dir hit-vec)
	   (if (not (gficl:button-down :left))
	       (progn
		 (setf *game-speed* 1)
		 (setf selected nil)
		 (setf velocity (gficl:*vec 3 hit-vec))
		 (setf angular-velocity (* 0.01 (gficl:magnitude hit-vec))))))
       (progn
	 (if (contains rect (gficl:make-vec (list mx my)))
	     (progn
	       (setf colour (gficl:make-vec '(0.5 0.5 0.5 1)))
	       (if (gficl:button-down :left)
		   (progn
		     (setf selected t)
		     (setf *game-speed* 0.1)
		     (setf colour (gficl:make-vec '(0.96 0.96 0.7 1))))))
	   (setf colour (gficl:make-vec '(1 1 1 1))))))
     (call-next-method))))

(defmethod collision ((obj1 player) (obj2 dream-object))
  (with-slots ((c1 collider) (v1 velocity) (m1 mass) (a1 angular-velocity)) obj1
    (with-slots ((c2 collider) (v2 velocity) (m2 mass) (a2 angular-velocity)) obj2
      (let ((col (collision c1 c2)))
	(cond (col
	       (setf v1 (gficl:make-vec '(0 0)))))
	col))))

;;; BG
(defclass bg-object (game-object)
  ((scale :initarg :scale :initform 1.0 :type float)
   (size :initarg :size :initform 1.0 :type float)
   (uv-model :initform (gficl:make-matrix) :type matrix)))

(defun make-bg-obj (x y width height depth tex-asset &optional (scale 1.0) (size 1.0))
  (let ((obj (make-game-obj x y width height depth tex-asset 0 :type 'bg-object)))
    (with-slots ((s scale) (sz size)) obj
      (setf s scale)
      (setf sz size)
      obj)))

(defmethod update-model ((obj bg-object))
  (call-next-method)
  (with-slots (rect depth scale uv-model size) obj
    (destructuring-bind
     (x y width height) (gficl:vec-data rect)
     (declare (ignore x y))
     (setf uv-model
	   (let ((sx (+ (* *x* scale) 800))
		 (sy (+ (* -1 *y* scale) 450))
		 (z (expt *zoom* (- scale)))
		 (ratio (/ width height)))
	     (gficl:*mat
	      (gficl:scale-matrix (list size (- size) 1))
	      (gficl:translation-matrix (list (/ (* 1 sx) (* width 0.5))
					      (/ sy (* height 0.5)) depth))
	      (gficl:scale-matrix (list (* 1 z) z 1))))))))
