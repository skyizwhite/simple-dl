(defpackage :gauna
  (:use #:numcl)
  (:nicknames #:gauna/pkg))
(in-package :gauna)

(defclass g-variable ()
  ((data    :accessor g-variable-data
            :initarg  :data)
   (grad    :accessor g-variable-grad
            :initform nil)
   (creator :accessor g-variable-creator
            :initform nil)))

(defun g-variable (data)
  (unless (or (null data)
              (and (numcl-array-p data)
                   (shape data)))
    (error "~a is not supported" (type-of data)))
  (make-instance 'g-variable :data data))

(defmethod set-creator ((v g-variable) f)
  (setf (g-variable-creator v) f))

(defgeneric backward (node &optional gy))

(defmethod backward ((v g-variable) &optional gy)
  (declare (ignore gy))
  (unless (g-variable-grad v)
    (setf (g-variable-grad v) (ones-like (g-variable-data v))))
  (let ((funcs (list (g-variable-creator v))))
    (loop :while funcs
          :for f := (pop funcs)
          :for x := (g-function-input f)
          :for y := (g-function-output f)
          :do (setf (g-variable-grad x) (backward f (g-variable-grad y)))
              (when (g-variable-creator x)
                (push (g-variable-creator x) funcs)))))

(defclass g-function () 
  ((input  :accessor g-function-input
           :initform nil)
   (output :accessor g-function-output
           :initform nil)))

(defmethod forward ((f g-function) x)
  (error "Not Implemented Error"))

(defun %g-fun (class input)
  (let* ((f (make-instance class))
         (x (g-variable-data input))
         (y (forward f x))
         (output (g-variable (asarray y))))
    (set-creator output f)
    (setf (g-function-input f) input
          (g-function-output f) output)
    output))

(defmacro def-g-fun (name &key forward backward)
  `(list (defclass ,name (g-function) ())
         (defun ,name (input) (%g-fun ',name input))
         (defmethod forward ((f ,name) ,@(first forward))
           ,@(rest forward))
         (defmethod backward ((f ,name) &optional ,@(first backward))
           ,@(rest backward))))

(defmethod input-data ((f g-function))
  (g-variable-data (g-function-input f)))

(def-g-fun g-square
  :forward ((x) (expt x 2))
  :backward ((gy) (let ((x (input-data f)))
                    (* 2 x gy))))

(def-g-fun g-exp
  :forward ((x) (exp x))
  :backward ((gy) (let ((x (input-data f)))
                    (* (exp x) gy))))
