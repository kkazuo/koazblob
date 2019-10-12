(defpackage koazblob/tests/main
  (:use :cl
        :koazblob
        :rove))
(in-package :koazblob/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :koazblob)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
