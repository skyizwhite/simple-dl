(uiop:define-package :gauna
  (:nicknames #:gauna/pkg)
  (:use #:numcl)
  (:import-from #:alexandria
                #:symbolicate)
  (:export #:g-variable
           #:make-g-variable
           #:@data
           #:@grad
           #:backward
           #:g-square
           #:d-exp))
(in-package :gauna)

(defclass g-variable ()
  ((data       :accessor @data
               :initarg  :data)
   (grad       :accessor @grad
               :initform nil)
   (creator    :accessor @creator
               :initform nil)
   (generation :accessor @generation
               :initform 0)))

(defun make-g-variable (data)
  (unless (or (null data)
              (and (numcl-array-p data)
                   (shape data)))
    (error "~a is not supported" (type-of data)))
  (make-instance 'g-variable :data data))

(defun g-asarray (arr)
  (make-g-variable (asarray arr)))

(defmethod set-creator ((v g-variable) f)
  (setf (@creator v) f)
  (setf (@generation v) (+ 1 (@generation f))))

(defmethod clear-grad ((v g-variable))
  (setf (@grad v) nil))

(defmethod backward ((v g-variable) &rest gy)
  (declare (ignore gy))
  (unless (@grad v)
    (setf (@grad v) (ones-like (@data v))))
  (let ((funcs (list))
        (seen-set (list)))
    (labels ((add-func (f)
               (unless (member f seen-set)
                 (push f funcs)
                 (push f seen-set)
                 (stable-sort funcs #'> :key #'@generation))))
      (add-func (@creator v))
      (loop :while funcs
            :for f := (pop funcs)
            :for gys := (mapcar #'@grad (@outputs f))
            :for gxs := (apply #'backward `(,f ,@gys))
            :do (unless (listp gxs) (setf gxs (list gxs)))
                (loop :for x :in (@inputs f)
                      :for gx :in gxs
                      :do (if (null (@grad x))
                              (setf (@grad x) gx)
                              (setf (@grad x) (+ (@grad x) gx)))
                          (when (@creator x)
                            (add-func (@creator x))))))))

(defclass g-function () 
  ((inputs  :accessor @inputs
            :initform nil)
   (outputs :accessor @outputs
            :initform nil)
   (generation :accessor @generation)))

(defmethod call ((f g-function) &rest inputs)
  (let* ((xs (mapcar #'@data inputs))
         (ys (let ((tmp (apply #'forward `(,f ,@xs))))
               (if (listp tmp) tmp (list tmp))))
         (outputs (mapcar (lambda (y) (make-g-variable (asarray y))) ys)))
    (setf (@generation f) (apply #'max (mapcar #'@generation inputs)))
    (mapc (lambda (output) (set-creator output f)) outputs)
    (setf (@inputs f) inputs
          (@outputs f) outputs)
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
                      (let ((xs (mapcar #'@data (@inputs f))))
                        (declare (ignorable xs))
                        ,@(rest backward))))))))

(def-g-fun g-square
  :forward ((x) (expt x 2))
  :backward ((gy) (let ((x (first xs)))
                    (* 2 x gy))))

(def-g-fun g-exp
  :forward ((x) (exp x))
  :backward ((gy) (let ((x (first xs)))
                    (* (exp x) gy))))

(def-g-fun g-add
  :forward ((x0 x1) (+ x0 x1))
  :backward ((gy) (list gy gy)))
