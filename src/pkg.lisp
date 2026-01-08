(uiop:define-package :gauna
  (:nicknames #:gauna/pkg)
  (:use #:numcl)
  (:import-from #:alexandria
                #:symbolicate)
  (:export #:g-variable
           #:make-g-variable
           #:g-variable-data
           #:g-variable-grad
           #:backward
           #:g-square
           #:d-exp))
(in-package :gauna)

(defclass g-variable ()
  ((data    :accessor g-variable-data
            :initarg  :data)
   (grad    :accessor g-variable-grad
            :initform nil)
   (creator :accessor g-variable-creator
            :initform nil)))

(defun make-g-variable (data)
  (unless (or (null data)
              (and (numcl-array-p data)
                   (shape data)))
    (error "~a is not supported" (type-of data)))
  (make-instance 'g-variable :data data))

(defmethod set-creator ((v g-variable) f)
  (setf (g-variable-creator v) f))

(defmethod backward ((v g-variable) &rest gy)
  (declare (ignore gy))
  (unless (g-variable-grad v)
    (setf (g-variable-grad v) (ones-like (g-variable-data v))))
  (let ((funcs (list (g-variable-creator v))))
    (loop :while funcs
          :for f := (pop funcs)
          :for x := (g-function-inputs f)
          :for y := (g-function-outputs f)
          :do (setf (g-variable-grad x) (backward f (g-variable-grad y)))
              (when (g-variable-creator x)
                (push (g-variable-creator x) funcs)))))

(defclass g-function () 
  ((inputs  :accessor g-function-inputs
            :initform nil)
   (outputs :accessor g-function-outputs
            :initform nil)))

(defmethod call ((f g-function) &rest inputs)
  (let* ((xs (mapcar #'g-variable-data inputs))
         (ys (let ((tmp (apply #'forward `(,f ,@xs))))
               (if (listp tmp) tmp (list tmp))))
         (outputs (mapcar (lambda (y) (make-g-variable (asarray y))) ys)))
    (mapcan (lambda (output) (set-creator output f)) outputs)
    (setf (g-function-inputs f) inputs)
    (setf (g-function-outputs f) outputs)
    (if (null (rest outputs))
        (first outputs)
        outputs)))

(defmacro def-g-fun (name &key forward backward)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (list (defclass ,name (g-function) ())
           (defun ,(symbolicate 'make- name) ()
             (make-instance ',name))
           (defun ,name (&rest inputs)
             (apply #'call `(,(make-instance ',name) ,@inputs)))
           ,(and forward
                 `(defmethod forward ((f ,name) &rest args)
                    (destructuring-bind ,(first forward) args
                      ,@(rest forward))))
           ,(and backward
                 `(defmethod backward ((f ,name) &rest args)
                    (destructuring-bind ,(first backward) args
                      ,@(rest backward)))))))

(defmethod inputs-data ((f g-function))
  (g-variable-data (g-function-inputs f)))

(def-g-fun g-square
  :forward ((x) (expt x 2))
  :backward ((gy) (let ((x (inputs-data f)))
                    (* 2 x gy))))

(def-g-fun g-exp
  :forward ((x) (exp x))
  :backward ((gy) (let ((x (inputs-data f)))
                    (* (exp x) gy))))

(def-g-fun g-add
  :forward ((x0 x1) (+ x0 x1)))
 