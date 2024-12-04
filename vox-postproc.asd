(defsystem :vox-postproc
  :name :vox-postproc
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Postprocessing after mariogeiger/obj2voxel"
  :licence "2-clause BSD"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "convert"))
  :depends-on (:command-line-parse
               :serapeum
               :numpy-npy
               :binary-media-gen)
  :build-operation program-op
  :build-pathname "vox-postproc"
  :entry-point "vox-postproc:main"
  :in-order-to ((test-op (load-op "vox-postproc/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :vox-postproc/tests))))))

(defsystem :vox-postproc/tests
  :name :vox-postproc/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:vox-postproc :fiveam))
