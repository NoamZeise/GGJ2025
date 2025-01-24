(in-package :ggj2025)

(defclass main-shader (fw:shader) ())

(defmethod fw:reload ((s main-shader))
  (fw:shader-reload-files (s (#p"main.vs" #p"main.fs")) shader
     ()))

(defmethod fw:draw ((s main-shader) scene)
  (call-next-method))

(defmethod fw:shader-scene-props ((s main-shader) (scene scene))
  (with-slots (viewproj) scene
    (with-slots ((shader fw:shader)) s
      (gficl:bind-matrix shader "viewproj" viewproj))))

(defmethod fw:shader-model-props ((s main-shader) (o object))
  (with-slots (model) o
    (with-slots ((shader fw:shader)) s      
      (gficl:bind-matrix shader "model" model))))

(defclass main-pass (fw:pass) ())

(defun make-main-pass ()
  (make-instance
   'main-pass
   :shaders (list (make-instance 'main-shader))
   :description
   (fw:make-framebuffer-description
    (list (gficl:make-attachment-description)
	  (gficl:make-attachment-description :position :depth-attachment)))
   :samples 16))

(defclass main-pipeline (fw:pipeline) ())

(defun make-main-pipeline ()
  (make-instance
   'main-pipeline
   :passes (list (cons :main (make-main-pass)))))

(defmethod fw:draw ((pl main-pipeline) scenes)
  (fw:draw (fw:get-pass pl :main) scenes)
  (gficl:blit-framebuffers
   (fw:get-final-framebuffer (fw:get-pass pl :main)) nil
   (gficl:window-width) (gficl:window-height)))
