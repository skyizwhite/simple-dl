(uiop:define-package #:gauna/sketch
  (:use #:numcl
        #:gauna))
(in-package #:gauna/sketch)


(defparameter x (make-g-variable (asarray '(1.0))))
(defparameter y (make-g-variable (asarray '(1.0))))


(defun sphere (x y)
  (g+ (g-square x) (g-square y)))

(defparameter z0 (sphere x y))
;(backward z)
;(@grad x) ; 2.0
;(@grad y) ; 2.0


(defun matyas (x y)
  (g- (g* 0.26 (g+ (g-square x) (g-square y)))
      (g* 0.48 (g* x y))))

(defparameter z1 (matyas x y))
;(backward z1)
;(@grad x) ; 0.03999999
;(@grad y) ; 0.03999999

(defun goldstein-price (x y)
  (let* ((x2 (g-square x))
         (y2 (g-square y))
         (xy (g* x y))
         ;; (x + y + 1)^2
         (a2 (g-square (g+ x (g+ y 1))))
         ;; 19 - 14x + 3x^2 - 14y + 6xy + 3y^2
         (b  (g+
              (g- 19 (g* 14 x))
              (g+
               (g- (g* 3 x2) (g* 14 y))
               (g+ (g* 6 xy)
                   (g* 3 y2)))))
         ;; 1 + (x+y+1)^2 * B
         (term1 (g+ 1 (g* a2 b)))
         ;; (2x - 3y)^2
         (c2 (g-square (g- (g* 2 x) (g* 3 y))))
         ;; 18 - 32x + 12x^2 + 48y - 36xy + 27y^2
         (d  (g+
              (g- 18 (g* 32 x))
              (g+
               (g+ (g* 12 x2)
                   (g* 48 y))
               (g+ (g- 0 (g* 36 xy))
                   (g* 27 y2)))))
         ;; 30 + (2x-3y)^2 * D
         (term2 (g+ 30 (g* c2 d))))
    (g* term1 term2)))

(defparameter z2 (goldstein-price x y))
;(backward z2)
;(@grad x) ; -5376.0
;(@grad y) ; 8064.0
