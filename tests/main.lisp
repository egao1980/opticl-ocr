(defpackage opticl-ocr/tests/main
  (:use :cl
        :opticl-ocr
        :rove))
(in-package :opticl-ocr/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :opticl-ocr)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
