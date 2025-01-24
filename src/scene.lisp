(in-package :ggj2025)

(defclass object ()
  ((meshes :initarg :meshes)
   (model :initarg :model :type gficl:matrix)))

(defmethod fw:draw ((o object) shader)
  (with-slots (meshes model) o
    (fw:shader-model-props shader o)
    (loop for mesh in meshes do
	  (gficl:draw-vertex-data mesh))))

(defun make-object (meshes model)
  (if (not (listp meshes)) (setf meshes (list meshes)))
  (make-instance 'object :meshes meshes :model model))

(defun make-2d-mat (x y w h &optional (depth 0.1))
  (gficl:*mat
   (gficl:translation-matrix (list x y depth))
   (gficl:scale-matrix (list w h 1))))

(defclass scene ()
  ((view :type gficl:matrix)
   (proj :type gficl:matrix)
   (viewproj :type gficl:matrix)
   (objects :initarg :objects)))

(defmethod fw:resize ((s scene) w h)
  (with-slots (view proj viewproj) s
    (setf view (gficl:make-matrix))
    (setf proj (gficl:screen-orthographic-matrix w h))
    (setf viewproj (gficl:*mat proj view))))

(defmethod fw:update-scene ((s scene) dt)
	   )

(defmethod fw:draw ((scene scene) shader)
	   (fw:shader-scene-props shader scene)
	   (with-slots (objects) scene
	     (loop for o in objects do (fw:draw o shader))))

;;; Main Scene

(defclass main-scene (scene) ())

(defun make-main-scene ()
  (make-instance
   'main-scene
   :objects (list (make-object (fw:get-asset 'quad)
			       (make-2d-mat 100 100 200 300)))))

;;; Post Scene

(defclass dummy-object () ())

(defmethod fw:draw ((o dummy-object) shader)
  (fw:shader-model-props shader o)
  (gficl:bind-gl (fw:get-asset 'dummy-data))
  (gl:draw-arrays :triangles 0 3))

(defclass post-scene (scene)
  ((width :initarg :width)
   (height :initarg :height)
   (tex :initarg :tex)))

(defun make-post-scene (tex target-w target-h)
  (make-instance
   'post-scene
   :tex tex :width target-w :height target-h
   :objects (list (make-instance 'dummy-object))))

(defun update-post-scene-tex (post-scene tex)
  (setf (slot-value post-scene 'tex) tex))

(defmethod fw:resize ((s post-scene) w h)
  (with-slots (viewproj width height) s
    (setf viewproj (gficl:target-resolution-matrix width height w h))))
