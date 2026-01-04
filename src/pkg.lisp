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
