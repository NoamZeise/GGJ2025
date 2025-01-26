(in-package :ggj2025)

(defun run ()
  (setf trivial-main-thread:*on-error* #'invoke-debugger)
  (trivial-main-thread:with-body-in-main-thread () (program)))

(defun program ()
  (gficl:with-window
   (:title "GGJ 2025"
    :resize-callback #'resize-callback
    :width *target-width*
    :height *target-height*)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (render))
   (cleanup)))

(defun load-assets ()
  (fw:setup-asset-table)
  (fw:add-asset 'dummy-data
		(gficl:make-vertex-data
		 (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int))) '(((0))) '(0 0 0)))
  (fw:add-asset 'quad
		(gficl:make-vertex-data
		 (gficl:make-vertex-form
		  (list (gficl:make-vertex-slot 2 :float)
			(gficl:make-vertex-slot 2 :float)))
		 '(((0 0) (0 0))
		   ((1 0) (1 0))
		   ((1 1) (1 1))
		   ((0 1) (0 1)))
		 '(0 3 2 2 1 0)))
  (fw:load-image 'fairy #p"assets/Bubbalah.png")
  
  (fw:load-image 'bloog #p"assets/Bloog.png")  
  (fw:load-image 'orring #p"assets/Orring.png")
  (fw:load-image 'ringus #p"assets/Ringus.png")
  (fw:load-image 'gloogs #p"assets/Gloogs.png")

  (fw:load-image 'dreamb #p"assets/Dreamb.png")
  (fw:load-image 'dreamp #p"assets/Dreamp.png")
  (fw:load-image 'dreamg #p"assets/Dreamg.png")
  (fw:load-image 'dreamr #p"assets/Dreamr.png")

  (fw:load-image 'gloob #p"assets/Gloob.png")
  (fw:load-image 'eggbert #p"assets/Eggbert.png")
  (fw:load-image 'yumb #p"assets/Yumb.png")
  
  (fw:load-image 'arrow #p"assets/arrow.png")
  (fw:load-image 'sun #p"assets/sun.png")
  (fw:load-image 'bg0 #p"assets/bg0.png")
  (fw:load-image 'bg1 #p"assets/BGLayer.png")
  (fw:load-image 'bg-clouds1 #p"assets/bg-clouds1.png")
  (fw:load-image 'bg-stars #p"assets/bg-stars.png")
  (fw:load-image 'bg00 #p"assets/bg00.png")

  (fw:load-image 'noise #p"assets/noise.png"))

(defun create-pipelines ()
  (setf *main-pipeline* (make-main-pipeline *target-width* *target-height*)))

(defun create-scenes ()
  (setf *game* (make-game *target-width* *target-height*)))

(defun setup ()
  (fw:init-watched)
  (load-assets)
  (setf *signal-fn* nil)
  (create-pipelines)
  (create-scenes)
  (resize-callback (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test)
  (gl:front-face :cw)
  (gl:cull-face :front))

(defun cleanup-pipelines ()
  (fw:free *main-pipeline*))
  
(defun cleanup ()
  (cleanup-pipelines)
  (fw:cleanup-assets))

(defun resize-callback (w h)
  (fw:resize *main-pipeline* w h))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen t)))
    ;;(format t "fps: ~d~%" (round (/ 1 (float dt))))
    (update-obj *game* dt)
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((fw:process-watched)
	   (fw:reload *main-pipeline*)
	   (fw:set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (fw:draw *main-pipeline*
	    (get-active-scenes *game*))))

;;; signal running program functions

(defun signal-quit ()
  (glfw:set-window-should-close))

(defun signal-reload ()
  "manually trigger shader reload"
  (fw:set-all-modified))

(defun signal-fn-lambda (fn)
  (setf *signal-fn* fn))

(defmacro signal-fn (&body body)
  "call fn during next update loop"
  `(signal-fn-lambda (function (lambda () ,@body))))

(defun signal-recreate-scenes ()
  (signal-fn (create-scenes)))

(defun signal-recreate-pipelines ()
  (signal-fn (cleanup-pipelines) (create-pipelines)))

;;; Global Variables

(defparameter *main-pipeline* nil)

(defparameter *active-pipeline* nil)

(defparameter *signal-fn* nil)

(defparameter *game* nil)
