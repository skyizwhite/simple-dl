(uiop:define-package :gauna
  (:nicknames #:gauna/pkg)
  (:use #:numcl)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:trivial-garbage
                #:make-weak-pointer
                #:weak-pointer-value)
  (:export #:g-variable
           #:make-g-variable
           #:@data
           #:@grad
           #:backward
           #:g-square
           #:d-exp))
(in-package :gauna)

;;;; Config
(defparameter *enable-backprop* t)

(defmacro with-no-grad (&body body)
  `(let ((*enable-backprop* nil))
     ,@body))

(defclass g-variable ()
  ((data       :accessor @data
               :initarg  :data)
   (name       :accessor @name
               :initarg  :name
               :initform nil)
   (grad       :accessor @grad
               :initform nil)
   (creator    :accessor @creator
               :initform nil)
   (generation :accessor @generation
               :initform 0)))

(defmacro delegate (class accessor fns)
  `(progn
     ,@(loop :for fn :in fns
             :collect
                `(defmethod ,(symbolicate "@" fn) ((obj ,class))
                   (,fn (,accessor obj))))))

(defmethod print-object ((v g-variable) stream)
  (format stream "variable(~a)~%" (@data v)))

(delegate g-variable @data
          (shape size dtype length))
; ndim length

(defun supported-data-p (data)
  (or (null data)
      (and (numcl-array-p data) (shape data))))

(defun make-g-variable (data &optional name)
  (unless (supported-data-p data)
    (error "~a is not supported" (type-of data)))
  (make-instance 'g-variable :data data :name name))

(defun g-asarray (arr)
  (make-g-variable (asarray arr)))

(defmethod set-creator ((v g-variable) f)
  (setf (@creator v) f
        (@generation v) (+ 1 (@generation f))))

(defmethod clear-grad ((v g-variable))
  (setf (@grad v) nil))

(defun ensure-list (l)
  (if (listp l) l (list l)))

(defun maybe-unlist (a)
  (if (null (rest a)) (first a) a))

(defmethod backward ((v g-variable) &key (retain-grad nil))
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
            :for gys := (mapcar #'(lambda (output)
                                    (@grad (weak-pointer-value output)))
                                (@outputs f))
            :for gxs := (ensure-list (apply #'backward f gys))
            :do (loop :for x :in (@inputs f)
                      :for gx :in gxs
                      :do (if (null (@grad x))
                              (setf (@grad x) gx)
                              (setf (@grad x) (+ (@grad x) gx)))
                          (when (@creator x)
                            (add-func (@creator x))))
                (unless retain-grad
                  (loop :for y in (@outputs f)
                        :do (setf (@grad (weak-pointer-value y)) nil)))))))

(defclass g-function () 
  ((inputs     :accessor @inputs
               :initform nil)
   (outputs    :accessor @outputs
               :initform nil)
   (generation :accessor @generation)))

(defmethod call ((f g-function) &rest inputs)
  (let* ((xs (mapcar #'@data inputs))
         (ys (ensure-list (apply #'forward f xs)))
         (outputs (mapcar (lambda (y) (make-g-variable (asarray y))) ys)))
    (when *enable-backprop*
      (setf (@generation f) (apply #'max (mapcar #'@generation inputs)))
      (mapc (lambda (output) (set-creator output f)) outputs)
      (setf (@inputs f) inputs
            (@outputs f) (mapcar #'make-weak-pointer outputs)))
    (maybe-unlist outputs)))

(defmacro def-g-fun (name &key forward backward)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name (g-function) ())
     (defun ,(symbolicate 'make- name) ()
       (make-instance ',name))
     (defun ,name (&rest inputs)
       (apply #'call (make-instance ',name) inputs))
     ,(and forward
           `(defmethod forward ((f ,name) &rest args)
              (destructuring-bind ,(first forward) args
                ,@(rest forward))))
     ,(and backward
           `(defmethod backward ((f ,name) &rest args)
              (destructuring-bind ,(first backward) args
                (let ((xs (mapcar #'@data (@inputs f))))
                  (declare (ignorable xs))
                  ,@(rest backward)))))))

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
