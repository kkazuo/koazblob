(defpackage koazblob/tests/main
  (:use :cl
        :koazblob
        :rove))
(in-package :koazblob/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :koazblob)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

(deftest test-util
  (testing "map key"
    (ok (equal (koazblob::alist-map-key '(("a" . "b")) 'string-upcase)
               '(("A" . "b")))))
  (testing "join"
    (ok (equal (koazblob::alist-join '(("a" . 1) ("b" . 2)) '("a" . 2)
                                     :test 'equal)
               '(("a" . 2) ("b" . 2))))
    (ok (equal (koazblob::alist-join '(("a" . 1) ("b" . 2)) '("c" . 3)
                                     :test 'equal)
               '(("c" . 3) ("a" . 1) ("b" . 2))))
    (ok (equal (koazblob::alist-join '() '("c" . 3)
                                     :test 'equal)
               '(("c" . 3))))
    (ok (equal (koazblob::alist-join '(("c" . 3)) '()
                                     :test 'equal)
               '(("c" . 3))))
    (ok (equal (koazblob::alist-join '() '()
                                     :test 'equal)
               '())))
  (testing "merge"
    (ok (equal (koazblob::alist-merge '(("a" . 1) ("b" . 2)) '(("a" . 2))
                                     :test 'equal)
               '(("a" . 2) ("b" . 2))))
    (ok (equal (koazblob::alist-merge '(("a" . 1) ("b" . 2)) '(("a" . 2) ("c" . 3))
                                     :test 'equal)
               '(("c" . 3) ("a" . 2) ("b" . 2))))
    (ok (equal (koazblob::alist-merge '() '(("c" . 3))
                                     :test 'equal)
               '(("c" . 3))))
    (ok (equal (koazblob::alist-merge '(("c" . 3)) '()
                                     :test 'equal)
               '(("c" . 3))))
    (ok (equal (koazblob::alist-merge '() '()
                                     :test 'equal)
               '()))))
