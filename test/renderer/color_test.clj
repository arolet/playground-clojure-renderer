(ns renderer.color_test
    (:require [clojure.test :refer :all]
      [renderer.color :refer :all]))

(deftest testMakeColor
  (is (equal {:r 1 :g 1 :b 1} (->Color 1 1 1)))
  (is (equal {:r -1 :g 3 :b 5} (->Color -1 3 5)))
  (is (not (equal {:r -1 :g 3 :b 5} (->Color 3 5 -1))))
  )

(deftest testAddColor
  (is (equal (->Color 1.6 0.7 1.0) (add (->Color 0.9 0.6 0.75) (->Color 0.7 0.1 0.25))))
  )

(deftest testRemoveColor
  (is (equal (->Color 0.2 0.5 0.5) (removeColor (->Color 0.9 0.6 0.75) (->Color 0.7 0.1 0.25))))
  )

(deftest testMulColor
  (is (equal (->Color 0.4 0.6 0.8) (mul (->Color 0.2 0.3 0.4) 2)))
  )

(deftest testElemMul
  (is (equal (->Color 0.9 0.2 0.04) (elemMul (->Color 1 0.2 0.4) (->Color 0.9 1 0.1))))
  )
