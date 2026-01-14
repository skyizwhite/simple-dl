(uiop:define-package #:gauna-test/core-simple
  (:use #:numcl
        #:gauna/core-simple)
  (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:ng))
(in-package #:gauna-test/core-simple)

(defun arr1-eq (a b)
  (equal #*1 (= a b)))

;;;; --------------------------------------------
;;;; Basic variable tests
;;;; --------------------------------------------

(deftest variable-test
  (testing "as-variable converts number to g-variable"
    (let ((v (as-variable 3.0)))
      (ok (typep v 'g-variable))
      (ok (arr1-eq (@data v) (asarray '(3.0))))))
  
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
      (ok (arr1-eq (@data y) (asarray '(4.0))))
      (ok (null (@creator y))))))

;;;; --------------------------------------------
;;;; Function tests (forward + backward)
;;;; --------------------------------------------

(deftest square-test
  (testing "square"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (y (g-square x)))
      (ok (arr1-eq (@data y) (asarray '(9.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(6.0)))))))

(deftest add-test
  (testing "add"
    (let* ((x0 (make-g-variable (asarray '(2.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g+ x0 x1)))
      (ok (arr1-eq (@data y) (asarray '(5.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x0)) (asarray '(1.0))))
      (ok (arr1-eq (@data (@grad x1)) (asarray '(1.0)))))))

(deftest mul-test
  (testing "mul"
    (let* ((x0 (make-g-variable (asarray '(2.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g* x0 x1)))
      (ok (arr1-eq (@data y) (asarray '(6.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x0)) (asarray '(3.0))))
      (ok (arr1-eq (@data (@grad x1)) (asarray '(2.0)))))))

(deftest neg-test
  (testing "neg"
    (let* ((x (make-g-variable (asarray '(2.0))))
           (y (g-neg x)))
      (ok (arr1-eq (@data y) (asarray '(-2.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(-1.0)))))))

(deftest sub-test
  (testing "sub"
    (let* ((x0 (make-g-variable (asarray '(5.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g- x0 x1)))
      (ok (arr1-eq (@data y) (asarray '(2.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x0)) (asarray '(1.0))))
      (ok (arr1-eq (@data (@grad x1)) (asarray '(-1.0)))))))

(deftest div-test
  (testing "div"
    (let* ((x0 (make-g-variable (asarray '(6.0))))
           (x1 (make-g-variable (asarray '(3.0))))
           (y (g/ x0 x1)))
      (ok (arr1-eq (@data y) (asarray '(2.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x0)) (asarray (/ 1.0 3.0))))
      (ok (arr1-eq (@data (@grad x1)) (asarray (- (/ 2.0 3.0))))))))

(deftest exp-test
  (testing "exp"
    (let* ((x (make-g-variable (asarray '(0.0))))
           (y (g-exp x)))
      (ok (arr1-eq (@data y) (asarray '(1.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(1.0)))))))

(deftest expt-test
  (testing "expt"
    (let* ((x (make-g-variable (asarray '(2.0))))
           (y (g-expt x 3)))
      (ok (arr1-eq (@data y) (asarray '(8.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(12.0)))))))

(deftest sin-test
  (testing "sin"
    (let* ((x (make-g-variable (asarray '(0.0))))
           (y (g-sin x)))
      ;; forward: sin(0) = 0
      (ok (arr1-eq (@data y) (asarray '(0.0))))
      ;; backward: cos(0) = 1
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(1.0)))))))

(deftest cos-test
  (testing "cos"
    (let* ((x (make-g-variable (asarray '(0.0))))
           (y (g-cos x)))
      ;; forward: cos(0) = 1
      (ok (arr1-eq (@data y) (asarray '(1.0))))
      ;; backward: -sin(0) = 0
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(0.0)))))))

(deftest tanh-test
  (testing "tanh"
    (let* ((x (make-g-variable (asarray '(0.0))))
           (y (g-tanh x)))
      ;; forward: tanh(0) = 0
      (ok (arr1-eq (@data y) (asarray '(0.0))))
      ;; backward: d/dx tanh(x) = 1 - tanh(x)^2, so at 0 => 1
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(1.0)))))))

;;;; --------------------------------------------
;;;; Chain rule / graph behavior
;;;; --------------------------------------------

(deftest chain-rule-test
  (testing "y = square(x) + x"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (y (g+ (g-square x) x)))
      (ok (arr1-eq (@data y) (asarray '(12.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(7.0)))))))

(deftest grad-accumulation-test
  (testing "gradient accumulates"
    (let* ((x (make-g-variable (asarray '(4.0))))
           (y (g* x x)))
      (ok (arr1-eq (@data y) (asarray '(16.0))))
      (backward y)
      (ok (arr1-eq (@data (@grad x)) (asarray '(8.0)))))))

(deftest retain-grad-test
  (testing "retain-grad"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (a (g-square x))
           (y (g+ a (asarray '(1.0)))))
      (ok (arr1-eq (@data y) (asarray '(10.0))))
      (backward y :retain-grad t)
      (ok (arr1-eq (@data (@grad a)) (asarray '(1.0)))))))

;;;; --------------------------------------------
;;;; Benchmark functions
;;;; --------------------------------------------

(defun sphere (x y)
  (g+ (g-square x) (g-square y)))

(defun matyas (x y)
  (g- (g* 0.26 (g+ (g-square x) (g-square y)))
      (g* 0.48 (g* x y))))

(defun goldstein-price (x y)
  (let* ((x2 (g-square x))
         (y2 (g-square y))
         (xy (g* x y))
         (a2 (g-square (g+ x (g+ y 1))))
         (b  (g+
              (g- 19 (g* 14 x))
              (g+
               (g- (g* 3 x2) (g* 14 y))
               (g+ (g* 6 xy)
                   (g* 3 y2)))))
         (term1 (g+ 1 (g* a2 b)))
         (c2 (g-square (g- (g* 2 x) (g* 3 y))))
         (d  (g+
              (g- 18 (g* 32 x))
              (g+
               (g+ (g* 12 x2)
                   (g* 48 y))
               (g+ (g- 0 (g* 36 xy))
                   (g* 27 y2)))))
         (term2 (g+ 30 (g* c2 d))))
    (g* term1 term2)))

(defun make-xy (&optional (x0 1.0) (y0 1.0))
  (values (make-g-variable (asarray (list (float x0 1.0))))
          (make-g-variable (asarray (list (float y0 1.0))))))

(deftest sphere-benchmark-test
  (testing "sphere (1,1)"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (sphere x y)))
        (ok (arr1-eq (@data z) (asarray '(2.0))))
        (backward z)
        (ok (arr1-eq (@data (@grad x)) (asarray '(2.0))))
        (ok (arr1-eq (@data (@grad y)) (asarray '(2.0))))))))

(deftest matyas-benchmark-test
  (testing "matyas (1,1)"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (matyas x y)))
        (ok (arr1-eq (@data z) (asarray '(0.03999999))))
        (backward z)
        (ok (arr1-eq (@data (@grad x)) (asarray '(0.03999999))))
        (ok (arr1-eq (@data (@grad y)) (asarray '(0.03999999))))))))

(deftest goldstein-price-benchmark-test
  (testing "goldstein-price (1,1)"
    (multiple-value-bind (x y) (make-xy 1.0 1.0)
      (let ((z (goldstein-price x y)))
        (ok (arr1-eq (@data z) (asarray '(1876.0))))
        (backward z)
        (ok (arr1-eq (@data (@grad x)) (asarray '(-5376.0))))
        (ok (arr1-eq (@data (@grad y)) (asarray '(8064.0))))))))
