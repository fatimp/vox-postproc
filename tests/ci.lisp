(defun do-all()
  (ql:quickload :vox-postproc/tests)
  (uiop:quit
   (if (uiop:call-function "vox-postproc/tests:run-tests")
       0 1)))

(do-all)
