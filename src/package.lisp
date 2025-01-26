(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)
#+windows (deploy:define-library deploy::libwinpthread :dont-deploy t)

(defpackage ggj2025
  (:use :cl)
  (:local-nicknames (:fw :framework))
  (:export #:run #:program))
