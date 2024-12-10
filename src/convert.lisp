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

(sera:-> add-padding ((simple-array bit (* * *)) (integer 1))
         (values (simple-array bit (* * *)) &optional))
(defun add-padding (array padding)
  (let* ((side  (array-dimension array 0))
         (range (select:range padding (+ padding side)))
         (new-side (+ side (* padding 2)))
         (result (make-array (list new-side new-side new-side)
                             :element-type 'bit
                             :initial-element 0)))
    (when *verbose*
      (format *standard-output* "Cube side with padding = ~d~%" new-side))
    (setf (select:select result range range range) array)
    result))

(defparameter *cmd-line*
  (seq
   (optional
    (flag   :verbose
            :short       #\v
            :long        "verbose"
            :description "Be verbose")
    (option :padding "N"
            :short       #\p
            :long        "padding"
            :fn          #'parse-integer
            :description "Pad by N zero voxels from each side"))
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

(defun main ()
  (let* ((arguments (handle-arguments))
         (input     (%assoc :input   arguments))
         (output    (%assoc :output  arguments))
         (padding   (%assoc :padding arguments))
         (*verbose* (%assoc :verbose arguments))
         (array (binary-media-gen:only-one-cluster
                 (fix-array
                  (load-array input)))))
    (write-output
     (if padding (add-padding array padding) array)
     output)))
