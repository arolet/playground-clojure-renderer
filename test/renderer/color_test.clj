(ns renderer.color_test
    (:require [clojure.test :refer :all]
      [renderer.color :refer :all]))

(deftest testMakeColor
  (is (equal [1 1 1] (makeColor 1 1 1)))
  (is (equal [-1 3 5] (makeColor -1 3 5)))
  (is (not (equal [-1 3 5] (makeColor 3 5 -1))))
  )

(deftest testAddColor
  (is (equal (makeColor 1.6 0.7 1.0) (add (makeColor 0.9 0.6 0.75) (makeColor 0.7 0.1 0.25))))
  )

(deftest testRemoveColor
  (is (equal (makeColor 0.2 0.5 0.5) (removeColor (makeColor 0.9 0.6 0.75) (makeColor 0.7 0.1 0.25))))
  )

(deftest testMulColor
  (is (equal (makeColor 0.4 0.6 0.8) (mul (makeColor 0.2 0.3 0.4) 2)))
  )

(deftest testElemMul
  (is (equal (makeColor 0.9 0.2 0.04) (elemMul (makeColor 1 0.2 0.4) (makeColor 0.9 1 0.1))))
  )
