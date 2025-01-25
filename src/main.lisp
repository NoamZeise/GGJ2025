(in-package :ggj2025)

(defun run ()
  (setf trivial-main-thread:*on-error* #'invoke-debugger)
  (trivial-main-thread:with-body-in-main-thread () (program)))

(defun program ()
  (gficl:with-window
   (:title "GGJ 2025"
    :resize-callback #'resize-callback)
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
  (fw:load-image 'test #p"assets/test.png"))

(defun create-pipelines ()
  (setf *main-pipeline* (make-main-pipeline *target-width* *target-height*)))

(defun create-scenes ()
  (setf *main-scene* (make-main-scene))
  (fw:resize *main-scene* *target-width* *target-height*))

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
     (:f (gficl:toggle-fullscreen t))
     ;; (:m
     ;;  (setf *active-pipeline*
     ;; 	    (loop for ((k . _) . r) on *pipelines*
     ;; 		  when (equalp k *active-pipeline*)
     ;; 		  return
     ;; 		  (if r (caar r) (caar *pipelines*))))
     ;;  (format t "using ~a pipeline~%" *active-pipeline*))
     )
    (gficl:map-buttons-down
     (:left (format t "fps: ~d~%" (round (/ 1 (float dt)))))
     (:right (format t "~a~%" (gficl:mouse-pos))))
					;(format t "fps: ~d~%" (round (/ 1 (float dt))))
    (fw:update-scene *main-scene* dt)
    (cond (*signal-fn*
	   (funcall *signal-fn*)
	   (setf *signal-fn* nil)))
    (cond ((fw:process-watched)
	   (fw:reload *main-pipeline*)
	   (fw:set-all-unmodified)))))

(defun render ()
  (gficl:with-render
   (fw:draw *main-pipeline* (list *main-scene*))))

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

(defparameter *main-scene* nil)

(defparameter *signal-fn* nil)
