(in-package :vox-postproc)

(defvar *verbose* nil)

(define-condition wrong-output-type (error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "Output extension must be .raw or .npy"))))

(defun check-output-type (filename)
  (if (member (pathname-type (pathname filename))
              '("raw" "npy") :test #'string=)
      filename
      (error 'wrong-output-type)))

(declaim (inline flatten))
(defun flatten (data)
  (make-array (array-total-size data)
              :element-type (array-element-type data)
              :displaced-to data))

(defparameter *cmd-line*
  (seq
   (optional
    (flag :verbose :short #\v :long "verbose" :description "Be verbose"))
   (argument :input  "INPUT"  :description "Input file (in numpy array format)")
   (argument :output "OUTPUT" :description "Output file (can be .raw or .npy)"
             :fn #'check-output-type)))

(defun handle-arguments ()
  (handler-case
      (parse-argv *cmd-line*)
    (cmd-line-parse-error ()
      (print-usage *cmd-line*)
      (uiop:quit 1))
    (wrong-output-type (c)
      (princ c *error-output*)
      (uiop:quit 1))))

(sera:-> fix-array ((simple-array (unsigned-byte 8) (*)))
         (values (simple-array bit (* * *)) &optional))
(defun fix-array (data)
  (declare (optimize (speed 3)))
  (let* ((side  (icbrt (length data)))
         (array (make-array (list side side side) :element-type 'bit)))
    (when *verbose*
      (format *standard-output* "A cube with the size ~d~%" side))
    ;; Recopy
    (map-into (flatten array) #'identity data)
    array))

(sera:-> write-output ((simple-array bit (* * *)) (or string pathname))
         (values &optional))
(defun write-output (data filename)
  (let ((type (pathname-type (pathname filename))))
    (cond
      ((string= type "npy")
       (store-array data filename))
      ((string= type "raw")
       (with-open-file (output filename
                               :direction         :output
                               :if-exists         :supersede
                               :if-does-not-exist :create
                               :element-type      '(unsigned-byte 8))
         (write-sequence (flatten data) output)))))
  (values))

(defun main ()
  (let* ((arguments (handle-arguments))
         (input     (%assoc :input   arguments))
         (output    (%assoc :output  arguments))
         (*verbose* (%assoc :verbose arguments)))
    (write-output
     (binary-media-gen:only-one-cluster
      (fix-array
       (load-array input)))
     output)))
