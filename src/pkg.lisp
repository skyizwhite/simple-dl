(defpackage :simple-dl
  (:use #:cl
        #:simple-matrix)
  (:nicknames #:simple-dl/pkg))
(in-package :simple-dl)

(defclass d-variable ()
  ((data :initarg  :data
         :accessor d-variable-data)))

(defun d-variable (data)
  (make-instance 'd-variable :data data))

(defclass d-function () ())

(defmethod call ((f d-function) (input d-variable))
  (d-variable (forward f (d-variable-data input))))

(defmethod forward ((f d-function) x)
  (error "Not Implemented Error"))

(defmacro def-d-fun (name &key forward)
  `(list
    (defclass ,name (d-function) ())
    (defun ,name () (make-instance ',name))
    (defmethod forward ((f ,name) ,@(first forward))
      ,@(rest forward))))

(def-d-fun d-square
  :forward ((x) (* x x)))

(def-d-fun d-exp
  :forward ((x) (exp x)))

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

(def-d-fun d-f
  :forward ((x) (d-variable-data (call-> (d-variable x)
                                         (d-square)
                                         (d-exp)
                                         (d-square)))))

(let ((x (d-variable 0.5)))
  (numerical-diff (d-f) x))
