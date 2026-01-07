(uiop:define-package #:gauna-test/pkg
  (:use #:numcl
        #:gauna)
  (:import-from #:rove
                #:deftest
                #:testing
                #:ok))
(in-package #:gauna-test/pkg)

(deftest square-test
  (testing "forward"
    (let* ((x (g-variable (asarray '(2.0))))
           (y (g-square x)))
      (ok (= (asarray '(4.0)) (g-variable-data y)))))
  (testing "backward"
    (let* ((x (g-variable (asarray '(3.0))))
           (y (g-square x)))
      (backward y)
      (ok (= (asarray '(6.0)) (g-variable-grad x))))))
  