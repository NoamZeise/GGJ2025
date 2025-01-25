(in-package :ggj2025)

;;; Main Shader

(defclass main-shader (fw:shader) ())

(defmethod fw:reload ((s main-shader))
  (fw:shader-reload-files (s (#p"main.vs" #p"main.fs")) shader
     (gl:uniformi (gficl:shader-loc shader "tex") 0)))

(defmethod fw:draw ((s main-shader) scene)
  (gl:enable :depth-test)
  (gl:active-texture :texture0)
  (call-next-method))

(defmethod fw:shader-scene-props ((s main-shader) (scene scene))
  (with-slots (viewproj) scene
    (with-slots ((shader fw:shader)) s
      (gficl:bind-matrix shader "viewproj" viewproj))))

(defmethod fw:shader-model-props ((s main-shader) (o object))
  (with-slots (model tex colour) o
    (with-slots ((shader fw:shader)) s      
      (gficl:bind-matrix shader "model" model)
      (gficl:bind-vec shader "tint" colour)
      (gficl:bind-gl tex))))

;;; Main Pass

(defclass main-pass (fw:pass) ())

(defun make-main-pass ()
  (make-instance
   'main-pass
   :shaders (list (make-instance 'main-shader))
   :description
   (fw:make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :position :depth-attachment))    
    :samples 16)
   :clear-colour '(0.184 0.156 0.458 0.0)))

;;; Post Shader

(defclass post-shader (fw:shader) ())

(defmethod fw:reload ((s post-shader))
  (fw:shader-reload-files (s (#p"post.vs" #p"post.fs")
			     :folder (fw:shader-subfolder #p"post/"))
			  shader
    (gl:uniformi (gficl:shader-loc shader "screen") 0)))

(defmethod fw:shader-scene-props ((s post-shader) (scene post-scene))
  (with-slots (tex viewproj) scene
    (with-slots ((shader fw:shader)) s
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d tex)
      (gficl:bind-matrix shader "transform" viewproj))))

(defmethod fw:shader-model-props ((s post-shader) (o dummy-object)) ())

(defclass post-pass (fw:pass) ())

(defun make-post-pass ()
  (make-instance
   'post-pass
   :shaders (list (make-instance 'post-shader))
   :description
   (fw:make-framebuffer-description
    (list (gficl:make-attachment-description))
    :samples 16)
   :clear-colour '(0 0 0 0)))

(defmethod fw:draw ((pass post-pass) scene)
	   (error "Tried to draw an arbitrary scene with a POST-PASS. 
Can only draw a POST-SCENE with a POST-PASS"))

(defmethod fw:draw ((pass post-pass) (scene post-scene))
  (with-slots ((shaders fw:shaders)) pass
    (gl:disable :depth-test :cull-face)
    (loop for shader in shaders do (fw:draw shader scene))))

;;; Main Pipeline

(defclass main-pipeline (fw:pipeline)
  ((post-scene :initarg :post-scene :type post-scene)))

(defun make-main-pipeline (target-w target-h)
  (let* ((main-pass (make-main-pass)))
    (fw:resize main-pass target-w target-h)
    (let ((pl (make-instance
	       'main-pipeline
	       :passes (list (cons :main main-pass)
			     (cons :post (make-post-pass)))
	       :post-scene (make-post-scene (fw:get-pass-texture main-pass) target-w target-h))))
      (fw:resize main-pass target-w target-h)
      pl)))

(defmethod fw:resize ((pl main-pipeline) (w integer) (h integer))
  (fw:resize (fw:get-pass pl :post) w h)
  (with-slots (post-scene) pl
    (fw:resize post-scene w h)))

(defmethod fw:draw ((pl main-pipeline) scenes)
  (fw:draw (fw:get-pass pl :main) scenes)
  (with-slots (post-scene) pl
    (fw:draw (fw:get-pass pl :post) post-scene))
  (gficl:blit-framebuffers
   (fw:get-final-framebuffer (fw:get-pass pl :post)) nil
   (gficl:window-width) (gficl:window-height)))
