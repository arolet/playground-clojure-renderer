(ns renderer.color_test
    (:require [clojure.test :refer :all]
      [renderer.color :refer :all]))

(deftest test-make-color
  (is (equal [1 1 1] (make-color 1 1 1)))
  (is (equal [-1 3 5] (make-color -1 3 5)))
  (is (not (equal [-1 3 5] (make-color 3 5 -1))))
  )

(deftest test-add-color
  (is (equal (make-color 1.6 0.7 1.0) (add (make-color 0.9 0.6 0.75) (make-color 0.7 0.1 0.25))))
  )

(deftest test-remove-color
  (is (equal (make-color 0.2 0.5 0.5) (remove-color (make-color 0.9 0.6 0.75) (make-color 0.7 0.1 0.25))))
  )

(deftest test-mul-color
  (is (equal (make-color 0.4 0.6 0.8) (mul (make-color 0.2 0.3 0.4) 2)))
  )

(deftest test-elem-mul
  (is (equal (make-color 0.9 0.2 0.04) (elem-mul (make-color 1 0.2 0.4) (make-color 0.9 1 0.1))))
  )
