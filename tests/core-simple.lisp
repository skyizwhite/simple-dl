(uiop:define-package #:gauna-test/core-simple
  (:use #:numcl
        #:gauna/core-simple)
  (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:ng))
(in-package #:gauna-test/core-simple)

;;;; --------------------------------------------
;;;; Basic variable tests
;;;; --------------------------------------------

(deftest variable-test
  (testing "as-variable converts number to g-variable"
    (let ((v (as-variable 3.0)))
      (ok (typep v 'g-variable))
      (ok (= (@data v) (asarray '(3.0))))))
  
  (testing "make-g-variable accepts numcl array"
    (ok (typep (make-g-variable (asarray '(1.0 2.0)))
               'g-variable))))

(deftest clear-grad-test
  (testing "clear-grad resets gradient"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (y (g-square x)))
      (backward y)
      (ok (@grad x))
      (clear-grad x)
      (ok (null (@grad x))))))

(deftest with-no-grad-test
  (testing "with-no-grad disables backprop graph construction"
    (let* ((x (make-g-variable (asarray '(2.0))))
           (y (with-no-grad (g-square x))))
      (ok (= (@data y) (asarray '(4.0))))
      (ok (null (@creator y))))))

;;;; --------------------------------------------
;;;; Function tests
;;;; --------------------------------------------

(deftest square-test
  (testing "forward"
    (let* ((x (asarray '(2.0)))
           (y (g-square x)))
      (ok (= (@data y) (asarray '(4.0))))))
  
  (testing "backward"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (y (g-square x)))
      (backward y)
      ;; dy/dx = 2x
      (ok (= (@grad x) (asarray '(6.0)))))))

(deftest add-test
  (testing "forward"
    (let* ((x0 (asarray '(2.0)))
           (x1 (asarray '(3.0)))
           (y (g+ x0 x1)))
      (ok (= (@data y) (asarray '(5.0))))))
  
  (testing "backward"
    (let* ((x0 (make-g-variable (asarray '(2.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g+ x0 x1)))
      (backward y)
      (ok (= (@grad x0) (asarray '(1.0))))
      (ok (= (@grad x1) (asarray '(1.0)))))))

(deftest mul-test
  (testing "forward"
    (let* ((x0 (asarray '(2.0)))
           (x1 (asarray '(3.0)))
           (y (g* x0 x1)))
      (ok (= (@data y) (asarray '(6.0))))))
  
  (testing "backward"
    (let* ((x0 (make-g-variable (asarray '(2.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g* x0 x1)))
      (backward y)
      (ok (= (@grad x0) (asarray '(3.0))))
      (ok (= (@grad x1) (asarray '(2.0)))))))

(deftest neg-test
  (testing "forward"
    (let* ((x (asarray '(2.0)))
           (y (g-neg x)))
      (ok (= (@data y) (asarray '(-2.0))))))
  
  (testing "backward"
    (let* ((x (make-g-variable (asarray '(2.0))))
           (y (g-neg x)))
      (backward y)
      (ok (= (@grad x) (asarray '(-1.0)))))))

(deftest sub-test
  (testing "forward"
    (let* ((x0 (asarray '(5.0)))
           (x1 (asarray '(3.0)))
           (y (g- x0 x1)))
      (ok (= (@data y) (asarray '(2.0))))))
  
  (testing "backward"
    (let* ((x0 (make-g-variable (asarray '(5.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g- x0 x1)))
      (backward y)
      (ok (= (@grad x0) (asarray '(1.0))))
      (ok (= (@grad x1) (asarray '(-1.0)))))))

(deftest div-test
  (testing "forward"
    (let* ((x0 (asarray '(6.0)))
           (x1 (asarray '(3.0)))
           (y (g/ x0 x1)))
      (ok (= (@data y) (asarray '(2.0))))))
  
  (testing "backward"
    (let* ((x0 (make-g-variable (asarray '(6.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g/ x0 x1)))
      (backward y)
      (ok (= (@grad x0) (asarray (/ 1.0 3.0))))
      (ok (= (@grad x1) (asarray (- (/ 2.0 3.0))))))))

(deftest exp-test
  (testing "forward"
    (let* ((x (asarray '(0.0)))
           (y (g-exp x)))
      (ok (= (@data y) (asarray '(1.0))))))
  
  (testing "backward"
    (let* ((x (make-g-variable (asarray '(0.0))))
           (y (g-exp x)))
      (backward y)
      (ok (= (@grad x) (asarray '(1.0)))))))

(deftest expt-test
  (testing "forward"
    (let* ((x (asarray '(2.0)))
           (y (g-expt x 3)))
      (ok (= (@data y) (asarray '(8.0))))))
  
  (testing "backward"
    (let* ((x (make-g-variable (asarray '(2.0))))
           (y (g-expt x 3)))
      (backward y)
      (ok (= (@grad x) (asarray '(12.0)))))))

;;;; --------------------------------------------
;;;; Chain rule / graph behavior
;;;; --------------------------------------------

(deftest chain-rule-test
  (testing "y = square(x) + x  => dy/dx = 2x + 1"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (y (g+ (g-square x) x)))
      (backward y)
      (ok (= (@data y) (asarray '(12.0))))
      (ok (= (@grad x) (asarray '(7.0)))))))

(deftest grad-accumulation-test
  (testing "gradient accumulates when variable is used twice"
    (let* ((x (make-g-variable (asarray '(4.0))))
           (y (g* x x)))
      (backward y)
      ;; dy/dx = 2x = 8
      (ok (= (@grad x) (asarray '(8.0)))))))

(deftest retain-grad-test
  (testing "retain-grad keeps intermediate gradients"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (a (g-square x))
           (y (g+ a (asarray '(1.0)))))
      (backward y :retain-grad t)
      (ok (= (@grad a) (asarray '(1.0)))))))

;;;; -------------------------------------------------
;;;; Benchmark functions
;;;; -------------------------------------------------

(defun sphere (x y)
  (g+ (g-square x) (g-square y)))

(defun matyas (x y)
  (g- (g* 0.26 (g+ (g-square x) (g-square y)))
      (g* 0.48 (g* x y))))

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

(defun make-xy (&optional (x0 1.0) (y0 1.0))
  (values (make-g-variable (asarray (list (float x0 1.0))))
          (make-g-variable (asarray (list (float y0 1.0))))))

(deftest sphere-benchmark-test
  (testing "forward at (1,1): sphere=2"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (sphere x y)))
        (ok (= (@data z) (asarray '(2.0)))))))
  
  (testing "backward at (1,1): grad=(2,2)"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (sphere x y)))
        (backward z)
        (ok (= (@grad x) (asarray '(2.0))))
        (ok (= (@grad y) (asarray '(2.0))))))))

(deftest matyas-benchmark-test
  (testing "forward at (1,1): matyas=0.04"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (matyas x y)))
        ;; 0.26*(1^2+1^2) - 0.48*(1*1) = 0.52 - 0.48 = 0.04
        (ok (= (@data z) (asarray '(0.04)))))))
  
  (testing "backward at (1,1): grad=(0.04,0.04)"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (matyas x y)))
        (backward z)
        ;; ∂/∂x = 0.52x - 0.48y, ∂/∂y = 0.52y - 0.48x
        ;; at (1,1) => 0.04, 0.04
        (ok (= (@grad x) (asarray '(0.04))))
        (ok (= (@grad y) (asarray '(0.04))))))))

(deftest goldstein-price-benchmark-test
  (testing "forward at (1,1): goldstein-price=1876"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (goldstein-price x y)))
        (ok (= (@data z) (asarray '(1876.0)))))))
  
  (testing "backward at (1,1): grad=(-5376, 8064)"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (goldstein-price x y)))
        (backward z)
        (ok (= (@grad x) (asarray '(-5376.0))))
        (ok (= (@grad y) (asarray '(8064.0))))))))
