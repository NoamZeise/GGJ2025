;; assume https://github.com/NoamZeise/gficl is cloned in this folder
(load "gficl/gficl.asd")
;; framework code from:
;; https://github.com/NoamZeise/project/tree/955aba71fcee4eb628b1efa6d3d9942520169a9f
(load "framework.asd")
(require 'asdf)
(in-package :asdf-user)

(defsystem :ggj2025
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ggj2025"
  :entry-point "ggj2025:run"
  :depends-on (:framework)
  :components ((:module "src"
		:components
		((:file "package")
		 (:file "main")
		 (:file "scene")
		 (:file "pipeline")))))
