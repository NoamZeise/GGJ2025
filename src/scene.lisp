(in-package :ggj2025)

(defclass object ()
  ((meshes :initarg :meshes)
   (model :initform (gficl:make-matrix) :initarg :model :type gficl:matrix)
   (tex :initarg :tex :type gficl:texture)
   (tex-w :initarg :tex-w :type integer)
   (tex-h :initarg :tex-h :type integer)
   (colour :initform (gficl:make-vec '(1 1 1 1)) :type gficl:vec)
   (active :initform t)
   (hidden :initform nil)))

(defmethod fw:draw ((o object) shader)
  (with-slots (meshes model) o
    (fw:shader-model-props shader o)
    (loop for mesh in meshes do
	  (gficl:draw-vertex-data mesh))))

(defun make-object (meshes model tex-asset &optional (object-type 'object))
  (if (not (listp meshes)) (setf meshes (list meshes)))
  (let ((tex (cdr (assoc :tex tex-asset)))
	(w (cdr (assoc :width tex-asset)))
	(h (cdr (assoc :height tex-asset))))
    (make-instance object-type :meshes meshes :model model :tex tex :tex-w w :tex-h h)))

(defun update-object-model (object model)
  (with-slots ((m model)) object
    (setf m model)))

(defclass scene ()
  ((view :initform (gficl:make-matrix) :type gficl:matrix)
   (proj :type gficl:matrix)
   (viewproj :type gficl:matrix)
   (objects :initarg :objects)
   (time :initform 0.0 :type float)))

(defmethod fw:resize ((s scene) w h)
  (with-slots (view proj viewproj) s
    (setf proj (gficl:screen-orthographic-matrix w h))
    (setf viewproj (gficl:*mat proj view))))

(defun add-object (scene object)
  (with-slots (objects) scene
    (setf objects (cons object objects))))

(defgeneric set-view (scene view-mat))
(defmethod set-view ((scene scene) (view-mat gficl:matrix))
  (with-slots (view proj viewproj) scene
    (setf view view-mat)
    (setf viewproj (gficl:*mat proj view))))

(defmethod fw:update-scene ((s scene) dt)
 (with-slots (time objects) s
   (setf time (+ time dt))
   (setf objects
	 (loop for o in objects when
	       (slot-value o 'active)
	       collecting o))))

(defmethod fw:draw ((scene scene) shader)
  (fw:shader-scene-props shader scene)
  (with-slots (objects) scene
    (loop for o in objects
	  when (not (slot-value o 'hidden))
	  do (fw:draw o shader))))

;;; Main Scene

(defclass main-scene (scene)
  ())

(defun make-main-scene ()
  (make-instance
   'main-scene
   :objects (list)))

;;; BG Scene

(defclass bg-scene (scene) ())

(defun make-bg-scene ()
  (make-instance
   'bg-scene
   :objects (list)))

(defmethod set-view ((scene bg-scene) (view-mat gficl:matrix))
  (with-slots (view proj viewproj) scene
    (setf view view-mat)
    (setf viewproj proj)))

(defmethod fw:update-scene ((s bg-scene) dt)
  (call-next-method)
  (with-slots (objects) s
    (loop for o in objects do
	  (update-model o))))

;;; Dream Scene

(defclass dream-scene (scene)
  ())

(defun make-dream-scene ()
  (make-instance
   'dream-scene
   :objects (list)))

(defclass dream-bg (bg-scene dream-scene) ())

(defun make-dream-bg-scene ()
  (make-instance
   'dream-bg
   :objects (list)))

;;; Post Scene

(defclass dummy-object (object) ())

(defmethod fw:draw ((o dummy-object) shader)
  (fw:shader-model-props shader o)
  (gficl:bind-gl (fw:get-asset 'dummy-data))
  (gl:draw-arrays :triangles 0 3))

(defclass post-scene (scene)
  ((width :initarg :width)
   (height :initarg :height)
   (tex :initarg :tex)
   (noise :initarg :noise)))

(defun make-post-scene (tex noise target-w target-h)
  (make-instance
   'post-scene
   :tex tex :noise noise :width target-w :height target-h
   :objects (list (make-instance 'dummy-object))))

(defmethod fw:resize ((s post-scene) w h)
  (with-slots (viewproj width height) s
    (setf viewproj (gficl:target-resolution-matrix width height w h))
    (setf *inverse-target-mat* (gficl:inverse-matrix viewproj))))
