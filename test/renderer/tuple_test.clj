(ns renderer.tuple_test
    (:require [clojure.test :refer :all]
      [renderer.tuple :refer :all]))

(defn makeTestV []
      (makeVector 4.3 -4.2 3.1))
(defn makeTestP []
      (makePoint 4.3 -4.2 3.1))


(deftest testTupleConstructor
              (is (= (:x (makeTestV) 4.3)))
              (is (= (:y (makeTestV) -4.2)))
              (is (= (:z (makeTestV) 3.1)))
              (is (= (:w (makeTestV) 0.0)))
              (is (= (:w (makeTestP) 1.0)))
              )

(deftest testIsVector
              (is (isVector (makeTestV)))
              (is (not (isVector (makeTestP))))
              )

(deftest testIsPoint
              (is (isPoint (makeTestP)))
              (is (not (isPoint (makeTestV))))
              )

(deftest testTupleEqual
              (is (equal (makeTestP) (makeTestP)))
              (is (equal (makeTestV) (makeTestV)))
              (is (not (equal (makeTestP) (makeTestV))))
              (is (not (equal (makeTestV) (makeTestP))))
              (is (not (equal (makePoint 4.0 -4.2 3.1) (makeTestP))))
              (is (not (equal (makePoint 4.3 -0.2 3.1) (makeTestP))))
              (is (not (equal (makePoint 4.3 -4.2 0.1) (makeTestP))))
              (is (equal (makePoint 0.0 -4.0 3.1) (makePoint 0 -4 3.1)))
              (is (equal (makePoint 0.0 -4.0 3.1) (makePoint -0.0 -4.0 3.1)))
              (is (not (equal {:a 1} {:b 0})))
              )

(deftest testTupleAdd
  (is (equal (makePoint 1 1 6) (add (makePoint 3 -2 5) (makeVector -2 3 1))))
  (is (equal (makePoint 1 1 6) (add (makePoint 3 -2 5) (makePoint -2 3 1))))
  )

(deftest testTupleRemove
  (is (equal (makeVector 0 0 0) (removeTuple (makePoint 3 -2 5) (makePoint 3 -2 5))))
  (is (equal (makePoint 0 0 0) (removeTuple (makePoint 3 -2 5) (makeVector 3 -2 5))))
  (is (equal (makeVector 5 -5 4) (removeTuple (makePoint 3 -2 5) (makePoint -2 3 1))))
  )

(deftest testTupleMinus
  (is (equal (makeVector -1 3 -2) (minus (makeVector 1 -3 2))))
  (is (equal (makePoint -1 3 -2) (minus (makePoint 1 -3 2))))
  )

(deftest testTupleMul
  (is (equal (makeVector 2 2 2) (mul (makeVector 1 1 1) 2)))
  (is (equal (makePoint 5 -5 20) (mul (makePoint 2 -2 8) 2.5)))
  (is (equal (makePoint -18 6 3) (mul (makePoint 6 -2 -1) -3)))
  )

(deftest testTupleDiv
  (is (equal (makeVector 1 1 1) (div (makeVector -2 -2 -2) -2)))
  (is (equal (makePoint 7 -4 2) (div (makePoint 56 -32 16) 8)))
  (is (equal (makePoint 6 -2 -1) (div (makePoint -18 6 3) -3)))
  )

(deftest testDot
  (is (== 0 (dot (makeVector 1 0 0) (makeVector 0 1 0))))
  (is (== 0 (dot (makeVector 1 0 0) (makeVector 0 0 1))))
  (is (== 0 (dot (makeVector 0 1 0) (makeVector 0 0 1))))
  (is (== 1 (dot (makeVector 1 0 0) (makeVector 1 0 1))))
  (is (== 1 (dot (makeVector 1 2 -2) (makeVector 1 1 1))))
  (is (== 9 (dot (makeVector 1 2 -2) (makeVector 1 1 -3))))
  )

(deftest testCross
  (is (equal (makeVector 0 0 1) (cross (makeVector 1 0 0) (makeVector 0 1 0))))
  (is (equal (makeVector 0 0 -1) (cross (makeVector 0 1 0) (makeVector 1 0 0))))
  (is (equal (makeVector 1 0 0) (cross (makeVector 0 1 0) (makeVector 0 0 1))))
  (is (equal (makeVector -1 0 0) (cross (makeVector 0 0 1) (makeVector 0 1 0))))
  (is (equal (makeVector 0 1 0) (cross (makeVector 0 0 1) (makeVector 1 0 0))))
  (is (equal (makeVector 0 -1 0) (cross (makeVector 1 0 0) (makeVector 0 0 1))))
  )

(def sqrt3 (Math/sqrt 3))
(deftest testNorm
  (is (== 0 (norm (makeVector 0 0 0))))
  (is (== 0 (norm (makePoint 0 0 0))))
  (is (== 1 (norm (makeVector 1 0 0))))
  (is (== 1 (norm (makeVector 0 1 0))))
  (is (== 1 (norm (makeVector 0 0 1))))
  (is (== sqrt3 (norm (makeVector 1 1 1))))
  (is (== sqrt3 (norm (makePoint 1 1 1))))
  )

(def sqrt14_inv (/ 1 (Math/sqrt 14)))
(def sqrt3Inv (/ 1 sqrt3))
(deftest testNormalize
  (is (equal (makeVector 1 0 0) (normalize (makeVector 10 0 0))))
  (is (equal (makeVector -1 0 0) (normalize (makeVector -36 0 0))))
  (is (equal (makeVector sqrt3Inv sqrt3Inv sqrt3Inv) (normalize (makeVector 1 1 1))))
  (is (equal (makeVector sqrt14_inv (* 2 sqrt14_inv) (* 3 sqrt14_inv)) (normalize (makeVector 1 2 3))))
  )

(deftest testAllKeys
  (is (= '() (allKeys {} {})))
  (is (= '(:a) (allKeys {:a 1} {})))
  (is (= '(:a) (allKeys {:a 1} {:a 0})))
  (is (= '(:a :b) (allKeys {:a 1} {:b 0})))
  )

(deftest testSameKeys
  (is (sameKeys {} {}))
  (is (sameKeys {:a 1} {:a 1}))
  (is (sameKeys {:a 1 :b nil :w 4} {:a 0 :b 2 :w 4}))
  (is (not (sameKeys {:a 1 :w 4} {:a 0 :b 2 :w 4})))
  )