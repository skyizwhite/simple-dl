(defpackage :simple-dl
  (:use #:cl
        #:simple-matrix)
  (:nicknames #:simple-dl/pkg))
(in-package :simple-dl)

(defclass d-variable ()
  ((data    :accessor d-variable-data
            :initarg  :data)
   (grad    :accessor d-variable-grad
            :initform nil)
   (creator :accessor d-variable-creator
            :initform nil)))

(defun d-variable (data)
  (make-instance 'd-variable :data data))

(defmethod set-creator ((v d-variable) f)
  (setf (d-variable-creator v) f))

(defgeneric backward (node &optional gy))

(defmethod backward ((v d-variable) &optional gy)
  (declare (ignore gy))
  (let ((f (d-variable-creator v)))
    (when f
      (let ((x (d-function-input f)))
        (setf (d-variable-grad x) (backward f (d-variable-grad v)))
        (backward x)))))

(defclass d-function () 
  ((input  :accessor d-function-input
           :initform nil)
   (output :accessor d-function-output
           :initform nil)))

(defmethod call ((f d-function) (input d-variable))
  (let* ((x (d-variable-data input))
         (y (forward f x))
         (output (d-variable y)))
    (set-creator output f)
    (setf (d-function-input f) input)
    (setf (d-function-output f) output)
    output))

(defmethod forward ((f d-function) x)
  (error "Not Implemented Error"))

(defmacro def-d-fun (name &key forward backward)
  `(list (defclass ,name (d-function) ())
         (defun ,name () (make-instance ',name))
         (defmethod forward ((f ,name) ,@(first forward))
           ,@(rest forward))
         (defmethod backward ((f ,name) &optional ,@(first backward))
           ,@(rest backward))))

(def-d-fun d-square
  :forward ((x) (* x x))
  :backward ((gy) (let ((x (d-variable-data (d-function-input f))))
                    (* 2 x gy))))

(def-d-fun d-exp
  :forward ((x) (exp x))
  :backward ((gy) (let ((x (d-variable-data (d-function-input f))))
                    (* (exp x) gy))))

(defmacro call-> (x &rest steps)
  (reduce (lambda (form step)
            `(call ,step ,form))
          (reverse steps)
          :initial-value x))

(d-variable-data (call-> (d-variable 0.5)
                         (d-square)
                         (d-exp)
                         (d-square)))

(defun numerical-diff (f x &optional (eps 1e-4))
  (let* ((x0 (d-variable (- (d-variable-data x)
                            eps)))
         (x1 (d-variable (+ (d-variable-data x)
                            eps)))
         (y0 (call f x0))
         (y1 (call f x1)))
    (/ (- (d-variable-data y1) (d-variable-data y0))
       (* 2 eps))))

(numerical-diff (d-square) (d-variable 2.0))

;(def-d-fun d-f
;  :forward ((x) (d-variable-data (call-> (d-variable x)
;                                         (d-square)
;                                         (d-exp)
;                                         (d-square)))))

;(let ((x (d-variable 0.5)))
;  (numerical-diff (d-f) x))

