(uiop:define-package #:gauna-test/core-simple
  (:use #:numcl
        #:gauna/core-simple)
  (:import-from #:rove
                #:deftest
                #:testing
                #:ok))
(in-package #:gauna-test/core-simple)

(deftest square-test
  (testing "forward"
    (let* ((x (asarray '(2.0)))
           (y (g-square x)))
      (ok (= (asarray '(4.0)) (@data y)))))
  (testing "backward"
    (let* ((x (make-g-variable (asarray '(3.0))))
           (y (g-square x)))
      (backward y)
      (ok (= (asarray '(6.0)) (@grad x))))))
