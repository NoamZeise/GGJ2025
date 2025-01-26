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
	     (* (* dt (if (> focus 0) 2 8))
		(- focus (/ focus-amount (if (> focus 0) focus 1))))))
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
   (dreams :initarg :dreams)
   (main-scene :initarg :main-scene :type scene :accessor main-scene)
   (dream-scene :initarg :dream-scene :type scene :accessor dream-scene)
   (dream-bg-scene :initarg :dream-bg-scene :type scene)
   (dream-goals :initarg :dream-goals)
   (bg-scene :initarg :bg-scene :type scene :accessor bg-scene)
   (cam :initarg :cam :type cam)
   (nearest-obs :initform 0)
   (entering-dream :initform nil)
   (active-dream :initform nil)
   (goals-got :initform 0)
   (in-dream :initform nil)))

(defun get-active-scenes (game)
  (with-slots (in-dream main-scene dream-scene dream-bg-scene bg-scene) game
      (if in-dream
	  (list dream-bg-scene dream-scene)
	(list main-scene bg-scene))))

(defun make-game (width height)
  (let ((main-scene (make-main-scene))
	(dream-scene (make-dream-scene))
	(dream-bg-scene (setup-dream-bg width height))
	(bg-scene (setup-bg width height))
	(player (make-player 0 0))
	(player-aim (make-game-obj 0 0 20 20 -0.02 (fw:get-asset 'arrow) 0))
	(cam (make-instance 'cam)))
    ;; Main Scene
    
    (with-slots (hidden pivot) player-aim
      (setf hidden t)
      (setf pivot (gficl:make-vec '(5 0))))
    (fw:resize main-scene width height)    
    
    (add-object main-scene player)
    (add-object main-scene player-aim)

    (fw:resize dream-scene width height)
    (add-object dream-scene player)
    (add-object dream-scene player-aim)

    (update-cam-view cam)
    (set-view main-scene (view cam))
    (set-view bg-scene (view cam))
    (set-view dream-scene (view cam))
    (set-view dream-bg-scene (view cam))
    (fw:update-scene main-scene 0)
    (fw:update-scene bg-scene 0)
    (fw:update-scene dream-bg-scene 0)
    (fw:update-scene dream-scene 0)
    
    (make-instance
     'game
     :player player
     :cam cam
     :main-scene main-scene
     :dream-scene dream-scene
     :dream-bg-scene dream-bg-scene
     :bg-scene bg-scene
     :player-aim player-aim
     :colliders (list)
     :dream-goals (list)
     :dreams (list))))

(defgeneric update-obj (obj dt))

(defmethod update-obj ((g game) dt)
  (with-slots (in-dream) g
    (if in-dream
	(dream-update g dt)
      (main-scene-update g dt))))

(defun main-scene-update (g dt)
    (with-slots (cam player player-aim colliders dreams main-scene nearest-obs
		   entering-dream active-dream in-dream goals-got)
      g
    (let ((physics-dt (* dt *game-speed*)))

      (if (< (length dreams) 2)
	  (let ((d (gen-dream (get-cam-rect cam))))
	    (setf dreams (cons d dreams))
	    (add-object main-scene d)))

      (if (< (length colliders) 120)
	  (cond ((> nearest-obs (* 2 *target-width* *target-height*))
		 (let ((new-obs (gen-obstacles (get-cam-rect cam) 2)))
		   (loop for o in new-obs do (add-object main-scene o))
		   (setf colliders
			 (concatenate 'list new-obs colliders))))
		((< (length colliders) 50)
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
		      c)))
	(if (not entering-dream)
	    (setf dreams
		  (loop for d in dreams when (slot-value d 'active)
			collecting
			(let* ((pos (get-game-object-pos d))
			       (diff (gficl:-vec player-pos pos))
			       (sz (gficl:dot diff diff)))
			  (update-obj d physics-dt)
			  (cond ((collision player d)
				 (setf entering-dream 1)
				 (setf active-dream d))
				((> sz (* *target-width* *target-height* 8))
				 (setf (slot-value d 'active) nil)))
			  d))))))
    (if entering-dream
	(progn
	  (setf *game-speed* 0.1)
	  (setf entering-dream (- entering-dream (/ dt 4)))
	  (target cam (get-target player) 8 dt)
	  (if (< entering-dream 0)
	      (progn
		(setf entering-dream nil)
		(setf (slot-value active-dream 'active) nil)
		(setf in-dream t)
		(setf goals-got 0))))
      (target cam (get-target player) (if (selected player) 1 0) dt))
    (update-cam-view cam)
    (with-slots (bg-scene) g
      (set-view main-scene (view cam))
      (set-view bg-scene (view cam)))
    (fw:update-scene (main-scene g) dt)
    (fw:update-scene (bg-scene g) dt)))

(defun dream-update (g dt)
      (with-slots (cam player player-aim in-dream nearest-obs
		       goals-got
		       dream-scene dream-bg-scene entering-dream dream-goals)
      g
    (let ((physics-dt (* dt *game-speed*)))
      
      (if (and (not entering-dream) (> goals-got 3))
	  (progn
	    (setf goals-got 0)
	    (setf entering-dream 1)))

      (if (< (length dream-goals) 120)
	  (cond ((> nearest-obs (* 4 *target-width* *target-height*))
		 (let ((new-obs (gen-dream-goal (get-cam-rect cam) 2)))
		   (loop for o in new-obs do (add-object dream-scene o))
		   (setf dream-goals
			 (concatenate 'list new-obs dream-goals))))
		((< (length dream-goals) 10)
		 (let ((new-obs (gen-dream-goal (get-cam-rect cam) 2)))
		   (loop for o in new-obs do (add-object dream-scene o))
		   (setf dream-goals
			 (concatenate 'list new-obs dream-goals))))))
      
      (update-obj player physics-dt)
      (let* ((player-pos (get-game-object-pos player)))
	(setf dream-goals
	      (loop for c in dream-goals when (slot-value c 'active) collecting
		    (progn
		      (update-obj c physics-dt)
		      (let ((col (collision player c)))
			(if (typep c 'bubble)
			    (progn
			      (if col
				  (progn
				    (if (not (following c))
					(progn
					  (setf goals-got (+ 1 goals-got))
					  (setf (following c) t)))))
			      (if (following c)
				  (setf (following c) player-pos)))))		      
		      (loop for c2 in dream-goals when (not (eql c c2)) do
			    (collision c c2))
		      (let* ((pos (get-game-object-pos c))
			     (diff (gficl:-vec player-pos pos))
			     (d (gficl:dot diff diff)))
			(if (or (> nearest-obs d) (= nearest-obs 0))
			    (setf nearest-obs d))
			(if (> d (* *target-width* *target-height* 10))
			    (setf (slot-value c 'active) nil)))
		      c))))
      
      (update-player-arrow player player-aim))
    (if entering-dream
	(progn
	  (setf *game-speed* 0.1)
	  (setf entering-dream (- entering-dream (/ dt 4)))
	  (target cam (get-target player) 8 dt)
	  (if (< entering-dream 0)
	      (progn
		(setf entering-dream nil)
		(setf in-dream nil))))
      (target cam (get-target player) (if (selected player) 1 0) dt))
    
    (update-cam-view cam)
    (set-view dream-scene (view cam))
    (set-view dream-bg-scene (view cam))
    (fw:update-scene dream-scene dt)
    (fw:update-scene dream-bg-scene dt)))


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
    ;; (add-object bg-scene
    ;; 		(make-bg-obj 0 0
    ;; 			     *target-width* *target-height*
    ;; 			     -0.96 (fw:get-asset 'bg1) 0.2 0.8))
    ;; (add-object bg-scene
    ;; 		(make-bg-obj 0 0
    ;; 			     *target-width* *target-height*
    ;; 			     -0.97 (fw:get-asset 'bg1) 0.09 1.5))
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
			     -0.988 (fw:get-asset 'sun) 0 0.5))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.99 (fw:get-asset 'bg00) 0 0.5))
    bg-scene))

(defun setup-dream-bg (width height)
  (let ((bg-scene (make-dream-bg-scene)))
    (fw:resize bg-scene width height)
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.51 (fw:get-asset 'bg-stars) 1.5 2))
    (add-object bg-scene
		(make-bg-obj 0 0
			     *target-width* *target-height*
			     -0.95 (fw:get-asset 'bg1) 0.5 0.2))
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

(defun gen-dream (rect)
  (let ((obs (get-dream))
	(offset 0))
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
	obs)))))

(defun get-dream ()
  (let* ((width (+ 800.0 (- (random 200.0) 100.0)))
	 (objs (list
		`(make-noise-obj 0 0 ,width ,width -0.1 0.1
				 (fw:get-asset 'dreamb) 1 :type 'dream-object)
		`(make-noise-obj 0 0 ,width ,width -0.1 0.1
				 (fw:get-asset 'dreamp) 1 :type 'dream-object)
		`(make-noise-obj 0 0 ,width ,width -0.1 0.1
				 (fw:get-asset 'dreamg) 1 :type 'dream-object)
		`(make-noise-obj 0 0 ,width ,width -0.1 0.1
				 (fw:get-asset 'dreamr) 1 :type 'dream-object)))
	 (n (random (length objs))))
    (eval (nth n objs))))


(defun gen-dream-goal (rect n)
  (loop for i from 0 to n collecting
	(let ((obs (get-dream-goal))
	      (offset (random 100)))
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


(defun get-dream-goal ()
  (let* ((large (= 0 (random 5)))
	 (width (+ 80 (if large (random 100.0) (random 40.0))))
	 (objs (list
		`(make-game-obj 0 0 ,width ,width 0.1 (fw:get-asset 'gloob) 0.8 :mass-mult 0.15
				:type 'bubble)
		`(make-game-obj 0 0 ,width ,width 0.1 (fw:get-asset 'eggbert) 0.8 :mass-mult 0.2
				:type 'dream-goal)
		`(make-game-obj 0 0 ,width ,width 0.1 (fw:get-asset 'yumb) 0.8 :mass-mult 0.3
				:type 'dream-goal)))
	 (n (random (length objs))))
    (eval (nth n objs))))
