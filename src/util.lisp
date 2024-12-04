(in-package :vox-postproc)

(define-condition icbrt-inexact (error)
  ())

(serapeum:-> icbrt-boundaries ((integer 0))
             (values (integer 0) (integer 0) &optional))
(defun icbrt-boundaries (n)
  (labels ((%go (k)
             (let ((m (expt 2 k)))
               (if (> (expt m 3) n)
                   (values (expt 2 (1- k)) m)
                   (%go (1+ k))))))
    (%go 1)))

(serapeum:-> icbrt ((integer 1))
             (values (integer 1) &optional))
(defun icbrt (n)
  (multiple-value-bind (min max)
      (icbrt-boundaries n)
    (labels ((%go (min max)
               (let* ((half (floor (+ min max) 2))
                      (m (expt half 3)))
                 (cond
                   ((= m n) half)
                   ((or (= half min)
                        (= half max))
                    (error 'icbrt-inexact))
                   ((< m n) (%go half max))
                   (t       (%go min  half))))))
      (%go min max))))