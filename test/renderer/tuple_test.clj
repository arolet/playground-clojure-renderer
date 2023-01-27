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
              (is (= (makeTestP) (makeTestP)))
              (is (= (makeTestV) (makeTestV)))
              (is (not (= (makeTestP) (makeTestV))))
              (is (not (= (makeTestV) (makeTestP))))
              (is (not (= (makePoint 4.0 -4.2 3.1) (makeTestP))))
              (is (not (= (makePoint 4.3 -0.2 3.1) (makeTestP))))
              (is (not (= (makePoint 4.3 -4.2 0.1) (makeTestP))))
              )

(deftest testTupleAdd
              (is (= (makePoint 1 1 6) (add (makePoint 3 -2 5) (makeVector -2 3 1))))
              (is (= (makePoint 1 1 6) (add (makePoint 3 -2 5) (makePoint -2 3 1))))
              )
