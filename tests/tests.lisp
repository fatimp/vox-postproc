(in-package :vox-postproc/tests)

(defun run-tests ()
  (explain! (run 'utils)))

(def-suite utils :description "Test utility functions")
(in-suite utils)

(test icbrt/pass
  (loop repeat 1000
        for n = (random 10000) do
        (is (= n (icbrt (expt n 3))))))

(test icbrt/fail
  (loop repeat 1000
        for n = (random 10000) do
        (signals icbrt-inexact (icbrt (+ (expt n 3) 1)))
        (signals icbrt-inexact (icbrt (+ (expt n 3) 2)))))
