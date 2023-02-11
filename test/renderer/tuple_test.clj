(ns renderer.tuple_test
    (:require [clojure.test :refer :all]
      [renderer.tuple :refer :all]))

(def test-v (make-vector 4.3 -4.2 3.1))
(def test-p (make-point 4.3 -4.2 3.1))


(deftest test-tuple-constructor
              (is (= (nth test-v 0) 4.3))
              (is (= (nth test-v 1) -4.2))
              (is (= (nth test-v 2) 3.1))
              (is (= (nth test-v 3) 0.0))
              (is (= (nth test-p 3) 1.0))
              )

(deftest test-vector?
              (is (tuple-vector? test-v))
              (is (not (tuple-vector? test-p)))
              )

(deftest test-point?
              (is (point? test-p))
              (is (not (point? test-v)))
              )

(deftest test-equal?
              (is (equal? test-p test-p))
              (is (equal? test-v test-v))
              (is (not (equal? test-p test-v)))
              (is (not (equal? test-v test-p)))
              (is (not (equal? (make-point 4.0 -4.2 3.1) test-p)))
              (is (not (equal? (make-point 4.3 -0.2 3.1) test-p)))
              (is (not (equal? (make-point 4.3 -4.2 0.1) test-p)))
              (is (equal? (make-point 0.0 -4.0 3.1) (make-point 0 -4 3.1)))
              (is (equal? (make-point 0.0 -4.0 3.1) (make-point -0.0 -4.0 3.1)))
              (is (not (equal? '[1] '[2])))
              )

(deftest test-tuple-add
  (is (equal? (make-point 1 1 6) (add (make-vector 3 -2 5) (make-point -2 3 1))))
  (is (equal? (make-vector 1 1 6) (add (make-vector 3 -2 5) (make-vector -2 3 1))))
  )

(deftest test-tuple-remove
  (is (equal? (make-vector 0 0 0) (remove-tuple (make-point 3 -2 5) (make-point 3 -2 5))))
  (is (equal? (make-point 0 0 0) (remove-tuple (make-point 3 -2 5) (make-vector 3 -2 5))))
  (is (equal? (make-vector 5 -5 4) (remove-tuple (make-point 3 -2 5) (make-point -2 3 1))))
  )


(deftest test-tuple-mul
  (is (equal? (make-vector 2 2 2) (mul (make-vector 1 1 1) 2)))
  (is (equal? (make-vector 5 -5 20) (mul (make-vector 2 -2 8) 2.5)))
  (is (equal? (make-vector -18 6 3) (mul (make-vector 6 -2 -1) -3)))
  )

(deftest test-point-mul
  (is (equal? (make-point 2 2 2) (point-mul (make-point 1 1 1) 2)))
  (is (equal? (make-point 5 -5 20) (point-mul (make-point 2 -2 8) 2.5)))
  (is (equal? (make-point -18 6 3) (point-mul (make-point 6 -2 -1) -3)))
  )

(deftest test-tuple-minus
  (is (equal? (make-vector -1 3 -2) (minus (make-vector 1 -3 2))))
  (is (equal? (make-vector -1 3 -2) (minus (make-vector 1 -3 2))))
  )
(deftest test-toint-minus
  (is (equal? (make-point -1 3 -2) (pointMinus (make-point 1 -3 2))))
  (is (equal? (make-point -1 3 -2) (pointMinus (make-point 1 -3 2))))
  )


(deftest test-tuple-div
  (is (equal? (make-vector 1 1 1) (div (make-vector -2 -2 -2) -2)))
  (is (equal? (make-vector 7 -4 2) (div (make-vector 56 -32 16) 8)))
  (is (equal? (make-vector 6 -2 -1) (div (make-vector -18 6 3) -3)))
  )

(deftest test-point-div
  (is (equal? (make-point 1 1 1) (point-iv (make-point -2 -2 -2) -2)))
  (is (equal? (make-point 7 -4 2) (point-iv (make-point 56 -32 16) 8)))
  (is (equal? (make-point 6 -2 -1) (point-iv (make-point -18 6 3) -3)))
  )

(deftest test-dot
  (is (== 0 (dot (make-vector 1 0 0) (make-vector 0 1 0))))
  (is (== 0 (dot (make-vector 1 0 0) (make-vector 0 0 1))))
  (is (== 0 (dot (make-vector 0 1 0) (make-vector 0 0 1))))
  (is (== 1 (dot (make-vector 1 0 0) (make-vector 1 0 1))))
  (is (== 1 (dot (make-vector 1 2 -2) (make-vector 1 1 1))))
  (is (== 9 (dot (make-vector 1 2 -2) (make-vector 1 1 -3))))
  )

(deftest test-cross
  (is (equal? (make-vector 0 0 1) (cross (make-vector 1 0 0) (make-vector 0 1 0))))
  (is (equal? (make-vector 0 0 -1) (cross (make-vector 0 1 0) (make-vector 1 0 0))))
  (is (equal? (make-vector 1 0 0) (cross (make-vector 0 1 0) (make-vector 0 0 1))))
  (is (equal? (make-vector -1 0 0) (cross (make-vector 0 0 1) (make-vector 0 1 0))))
  (is (equal? (make-vector 0 1 0) (cross (make-vector 0 0 1) (make-vector 1 0 0))))
  (is (equal? (make-vector 0 -1 0) (cross (make-vector 1 0 0) (make-vector 0 0 1))))
  )

(def sqrt3 (Math/sqrt 3))
(deftest testNorm
  (is (== 0 (norm (make-vector 0 0 0))))
  (is (== 1 (norm (make-vector 1 0 0))))
  (is (== 1 (norm (make-vector 0 1 0))))
  (is (== 1 (norm (make-vector 0 0 1))))
  (is (== sqrt3 (norm (make-vector 1 1 1))))
  )

(deftest test-point-norm
  (is (== 0 (point-norm (make-point 0 0 0))))
  (is (== sqrt3 (point-norm (make-point 1 1 1))))
  )

(def sqrt14_inv (/ 1 (Math/sqrt 14)))
(def sqrt3Inv (/ 1 sqrt3))

(deftest test-normalize
  (is (equal? (make-vector 1 0 0) (normalize (make-vector 10 0 0))))
  (is (equal? (make-vector -1 0 0) (normalize (make-vector -36 0 0))))
  (is (equal? (make-vector sqrt3Inv sqrt3Inv sqrt3Inv) (normalize (make-vector 1 1 1))))
  (is (equal? (make-vector sqrt14_inv (* 2 sqrt14_inv) (* 3 sqrt14_inv)) (normalize (make-vector 1 2 3))))
  )

(deftest test-same-direction
  (is (same-direction? (make-vector 1 0 0) (make-vector 1 0 0)))
  (is (same-direction? (make-vector 10 0 0) (make-vector 1 0 0)))
  (is (same-direction? (make-vector 1 0.00001 0) (make-vector 1 0 0)))
  (is (same-direction? (make-vector 1 -1 1) (make-vector 3 -3 3)))
  (is (same-direction? (make-vector 1 -1 1) (make-vector 0 0 0)))
  (is (not (same-direction? (make-vector 1 0 0) (make-vector 0 1 0))))
  (is (not (same-direction? (make-vector 1 0 0) (make-vector -1 1 0))))
  (is (not (same-direction? (make-vector 1 1 0) (make-vector 1 -1 0))))
  )

(deftest test-average
  (is (equal? (make-point 0 0 0) (average (make-point 0 0 0))))
  (is (equal? (make-point 0.5 0 0) (average (make-point 0 0 0) (make-point 1 0 0))))
  (is (equal? (make-point 0 0.5 0) (average (make-point 0 0 0) (make-point 0 1 0))))
  (is (equal? (make-point 0.5 0 0.5) (average (make-point 0 0 0) (make-point 1 0 1))))
  (is (equal? (make-vector 0 (/ 1.0 3) 0) (average (make-vector 0 0 0) (make-vector 1 1 0) (make-vector -1 0 0))))
  )

(def sqrt2_2 (/ 1 (Math/sqrt 2)))

(deftest test-reflect
  (is (equal? (make-vector 1 1 0) (reflect (make-vector 1 -1 0) (make-vector 0 1 0))))
  (is (equal? (make-vector 1 0 0) (reflect (make-vector 0 -1 0) (make-vector sqrt2_2 sqrt2_2 0))))
  )
