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

(defclass main-scene (scene) ())

(defun make-main-scene ()
  (make-instance
   'main-scene
   :objects (list (make-object (fw:get-asset 'quad)
			       (make-2d-mat 100 100 200 300)))))
