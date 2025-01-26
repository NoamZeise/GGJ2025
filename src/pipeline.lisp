(in-package :ggj2025)

;;; Main Shader

(defclass main-shader (fw:shader) ())

(defmethod fw:reload ((s main-shader))
  (fw:shader-reload-files (s (#p"main.vs" #p"main.fs")) shader
     (gl:uniformi (gficl:shader-loc shader "tex") 0)
     (gl:uniformi (gficl:shader-loc shader "noise_tex") 1)))

(defmethod fw:draw ((s main-shader) scene)
  (gl:enable :depth-test :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:active-texture :texture0)
  (call-next-method))

(defmethod fw:shader-scene-props ((s main-shader) (scene scene))
  (with-slots (time viewproj) scene
    (with-slots ((shader fw:shader)) s
      (gl:uniformi (gficl:shader-loc shader "correct_uv") 0)
      (gl:uniformi (gficl:shader-loc shader "scene_noise") 0)
      (gl:uniformf (gficl:shader-loc shader "time") time)
      (gficl:bind-matrix shader "viewproj" viewproj))))

(defmethod fw:shader-model-props ((s main-shader) (o object))
  (with-slots (model tex colour) o
    (with-slots ((shader fw:shader)) s      
      (gficl:bind-matrix shader "model" model)
      (gficl:bind-vec shader "tint" colour)
      (gl:active-texture :texture0)
      (gl:uniformi (gficl:shader-loc shader "output_noise") 0)
      (gficl:bind-gl tex))))

(defmethod fw:shader-model-props ((s main-shader) (o game-object))
	   (call-next-method)
	   (with-slots (rect) o
	       (let ((w (gficl:vec-ref rect 2)))
		 (with-slots ((shader fw:shader)) s
		   (gl:uniformf (gficl:shader-loc shader "speed") 0)
		   (gl:uniformf (gficl:shader-loc shader "obj_size") 0)))))

(defmethod fw:shader-scene-props ((s main-shader) (scene bg-scene))
	   (with-slots (proj viewproj) scene
	     (with-slots ((shader fw:shader)) s
	       (gl:uniformi (gficl:shader-loc shader "correct_uv") 1)
	       (gl:uniformf (gficl:shader-loc shader "uv_speed") 0.5)
	       (gficl:bind-matrix shader "uv_mat" viewproj)
	       (gficl:bind-matrix shader "viewproj" proj))))

(defmethod fw:shader-scene-props ((s main-shader) (scene dream-scene))
  (call-next-method)
  (with-slots ((shader fw:shader)) s
    (gl:uniformi (gficl:shader-loc shader "scene_noise") 1)
    (gl:uniformf (gficl:shader-loc shader "speed") 0.01)))


(defmethod fw:shader-model-props ((s main-shader) (o bg-object))
	   (call-next-method)
	   (with-slots ((shader fw:shader)) s
	     (with-slots (uv-model) o
	       (gficl:bind-matrix shader "uv_model" uv-model)
	       (gl:active-texture :texture1)
	       (gficl:bind-gl (cdr (assoc :tex (fw:get-asset 'noise)))))
	     (with-slots (rect) o
	       (let ((w (gficl:vec-ref rect 2)))
		 (gl:uniformf (gficl:shader-loc shader "speed") 0.005)
		 (gl:uniformf (gficl:shader-loc shader "obj_size") w)))))

(defmethod fw:shader-model-props ((s main-shader) (o noise-object))
  (call-next-method)  
  (with-slots ((shader fw:shader)) s
    (with-slots (noise-speed) o
      (gl:uniformf (gficl:shader-loc shader "speed") noise-speed)
      (gl:uniformi (gficl:shader-loc shader "output_noise") 1)
      (gl:uniformf (gficl:shader-loc shader "obj_size") -400)
      (gl:active-texture :texture1)
      (gficl:bind-gl (cdr (assoc :tex (fw:get-asset 'noise)))))))

;;; Main Pass

(defclass main-pass (fw:pass) ())

(defun make-main-pass ()
  (make-instance
   'main-pass
   :shaders (list (make-instance 'main-shader))
   :description
   (fw:make-framebuffer-description
    (list (gficl:make-attachment-description :type :texture)
	  (gficl:make-attachment-description :type :texture :position :color-attachment1)
	  (gficl:make-attachment-description :position :depth-attachment))    
    :samples 1)
   :clear-colour '(0.184 0.156 0.458 0.0)))

;;; Post Shader

(defclass post-shader (fw:shader) ())

(defmethod fw:reload ((s post-shader))
  (fw:shader-reload-files (s (#p"post.vs" #p"post.fs")
			     :folder (fw:shader-subfolder #p"post/"))
			  shader
    (gl:uniformi (gficl:shader-loc shader "screen") 0)
    (gl:uniformi (gficl:shader-loc shader "noise") 1)))

(defmethod fw:shader-scene-props ((s post-shader) (scene post-scene))
  (with-slots (tex noise viewproj) scene
    (with-slots ((shader fw:shader)) s
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d tex)
      (gl:active-texture :texture1)
      (gl:bind-texture :texture-2d noise)
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
	       :post-scene (make-post-scene
			    (fw:get-pass-texture main-pass)
			    (fw:get-pass-texture main-pass :color-attachment1)
					    target-w target-h))))
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
