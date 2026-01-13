(uiop:define-package #:gauna/core-simple
  (:use #:numcl)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:trivial-garbage
                #:make-weak-pointer
                #:weak-pointer-value)
  (:export #:with-no-grad
           #:g-variable
           #:@name
           #:@data
           #:@grad
           #:@creator
           #:@shape
           #:@size
           #:@dtype
           #:@length
           #:as-variable
           #:make-g-variable
           #:clear-grad
           #:g-function
           #:def-g-fun
           #:backward
           #:g+
           #:g-
           #:g*
           #:g/
           #:g-neg
           #:g-exp
           #:g-square
           #:g-expt
           #:render-graph))
(in-package #:gauna/core-simple)

;;;; Config
(defparameter *enable-backprop* t)

(defmacro with-no-grad (&body body)
  `(let ((*enable-backprop* nil))
     ,@body))

(defclass g-variable ()
  ((id         :reader   @id
               :initform (string (gensym "V")))
   (data       :accessor @data
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

(delegate g-variable @data
          (shape size dtype length))
; ndim

(defmethod print-object ((v g-variable) stream)
  (format stream "variable(~a)~%" (@data v)))

(defun supported-data-p (data)
  (or (null data)
      (and (numcl-array-p data) (shape data))))

(defun make-g-variable (data &optional name)
  (unless (supported-data-p data)
    (error "~a is not supported" (type-of data)))
  (make-instance 'g-variable :data data :name name))

(defun as-variable (obj)
  (cond ((typep obj 'g-variable)
         obj)
        ((numberp obj)
         (make-g-variable (asarray (ensure-list obj))))
        (t
         (make-g-variable obj))))

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
            :for gxs := (ensure-list (apply #'f-backward f gys))
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
  ((id         :reader   @id
               :initform (string (gensym "F")))
   (inputs     :accessor @inputs
               :initform nil)
   (outputs    :accessor @outputs
               :initform nil)
   (generation :accessor @generation)))

(defmethod call ((f g-function) &rest inputs)
  (let* ((inputs (mapcar #'as-variable inputs))
         (xs (mapcar #'@data inputs))
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
     (defmethod forward ((f ,name) &rest args)
       (destructuring-bind ,(first forward) args
         ,@(rest forward)))
     (defmethod f-backward ((f ,name) &rest args)
       (destructuring-bind ,(first backward) args
         (destructuring-bind ,(first forward) (mapcar #'@data (@inputs f))
           (declare (ignorable ,@(first forward)))
           ,@(rest backward))))))

(def-g-fun g+
  :forward ((x0 x1) (+ x0 x1))
  :backward ((gy) (list gy gy)))

(def-g-fun g*
  :forward ((x0 x1) (* x0 x1))
  :backward ((gy) (list (* gy x1) (* gy x0))))

(def-g-fun g-neg
  :forward ((x) (- x))
  :backward ((gy) (- gy)))

(def-g-fun g-
  :forward ((x0 x1) (- x0 x1))
  :backward ((gy) (list gy (- gy))))

(def-g-fun g/
  :forward ((x0 x1) (/ x0 x1))
  :backward ((gy) (let ((gx0 (/ gy x1))
                        (gx1 (* gy (/ (- x0) (expt x1 2)))))
                    (list gx0 gx1))))

(def-g-fun g-expt
  :forward ((x c) (expt x c))
  :backward  ((gy) (* c (expt x (- c 1)) gy)))

(def-g-fun g-exp
  :forward ((x) (exp x))
  :backward ((gy) (* (exp x) gy)))

(def-g-fun g-square
  :forward ((x) (expt x 2))
  :backward ((gy) (* 2 x gy)))


;;;; utils

(defun dot-var (v &key verbose)
  (format nil "~a [label=\"~a\", color=orange, style=filled]~%"
          (@id v)
          (with-accessors ((name  @name)
                           (shape @shape)
                           (dtype @dtype)) v
            (if name
                (if verbose
                    (format nil "~a: ~a ~a" name shape dtype)
                    name)
                ""))))

(defun dot-func (f)
  (apply #'cl:concatenate 'string
         (format nil "~a [label=\"~a\", color=lightblue, style=filled, shape=box]~%"
                 (@id f) (class-name (class-of f)))
         (let ((fmt "~a -> ~a~%"))
           (append
            (loop :for x :in (@inputs f)
                  :collect (format nil fmt (@id x) (@id f)))
            (loop :for y :in (@outputs f)
                  :collect (format nil fmt (@id f) (@id (weak-pointer-value y))))))))

(defun get-dot-graph (v &key (verbose t))
  (let ((txt "")
        (funcs (list))
        (seen-set (list)))
    (labels ((add-func (f)
               (unless (member f seen-set)
                 (push f funcs)
                 (push f seen-set))))
      (add-func (@creator v))
      (setf txt (cl:concatenate 'string txt (dot-var v :verbose verbose)))
      (loop :while funcs
            :for f := (pop funcs)
            :do (setf txt (cl:concatenate 'string txt (dot-func f))) 
                (loop :for x :in (@inputs f)
                      :do (setf txt (cl:concatenate 'string txt (dot-var x :verbose verbose)))
                          (when (@creator x)
                            (add-func (@creator x))))))
    (format nil "digraph g {~%~a~%}" txt)))

(defun render-graph (output &key
                            (dot-path "graph.dot")
                            (png-path "graph.png")
                            (dot-command "dot")
                            (verbose nil))
  (let ((dot (get-dot-graph output :verbose verbose)))
    (unless (stringp dot)
      (error "get-dot-graph must return a string, got: ~S" dot))
    (with-open-file (out dot-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :external-format :utf-8)
      (write-string dot out))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list dot-command "-Tpng" dot-path "-o" png-path)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (declare (ignore stdout))
      (unless (and (integerp exit-code) (zerop exit-code))
        (error "dot command failed (exit=~A). stderr: ~A"
               exit-code
               (or stderr ""))))
      
    png-path))
