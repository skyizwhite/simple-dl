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
  (make-instance 'g-variable :data data))

(defmethod set-creator ((v g-variable) f)
  (setf (g-variable-creator v) f))

(defgeneric backward (node &optional gy))

(defmethod backward ((v g-variable) &optional gy)
  (declare (ignore gy))
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

(defmethod call ((f g-function) (input g-variable))
  (let* ((x (g-variable-data input))
         (y (forward f x))
         (output (g-variable y)))
    (set-creator output f)
    (setf (g-function-input f) input)
    (setf (g-function-output f) output)
    output))

(defmethod forward ((f g-function) x)
  (error "Not Implemented Error"))

(defmacro def-g-fun (name &key forward backward)
  `(list (defclass ,name (g-function) ())
         (defun ,name () (make-instance ',name))
         (defmethod forward ((f ,name) ,@(first forward))
           ,@(rest forward))
         (defmethod backward ((f ,name) &optional ,@(first backward))
           ,@(rest backward))))

(def-g-fun g-square
  :forward ((x) (* x x))
  :backward ((gy) (let ((x (g-variable-data (g-function-input f))))
                    (* 2 x gy))))

(def-g-fun g-exp
  :forward ((x) (exp x))
  :backward ((gy) (let ((x (g-variable-data (g-function-input f))))
                    (* (exp x) gy))))

(defmacro call-> (x &rest steps)
  (reduce (lambda (form step)
            `(call ,step ,form))
          (reverse steps)
          :initial-value x))
