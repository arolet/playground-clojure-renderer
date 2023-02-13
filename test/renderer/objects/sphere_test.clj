(ns renderer.objects.sphere_test
  (:require [clojure.test :refer :all]
            [renderer.constants :refer :all]
            [renderer.material :refer
             [make-material]]
            [renderer.objects.factory :refer [make-object]]
            [renderer.objects.objects :refer :all]
            [renderer.objects.sphere :refer :all]
            [renderer.ray :refer [get-point make-ray]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.utils :refer [close?]]))


(deftest test-normal-at-unit-sphere
  (is (equal? (make-vector 1 0 0) (normal-at-unit-sphere (make-point 1 0 0))))
  (is (equal? (make-vector 0 1 0) (normal-at-unit-sphere (make-point 0 1 0))))
  (is (equal? (make-vector 0 0 1) (normal-at-unit-sphere (make-point 0 0 1))))
  (is (equal? (make-vector sqrt2_2 sqrt2_2 0) (normal-at-unit-sphere (make-point sqrt2_2 sqrt2_2 0)))))

(deftest test-sphere-material
  (let [mater (make-material)
        sphere (make-object sphere-kw mater)]
    (is (= mater (get-material sphere)))))

(deftest test-intersect-unit-sphere
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point -2 0 0) (make-vector 1 0 0)))]
    (is (equal? (make-point -1 0 0) (get-point enter)))
    (is (equal? (make-point 1 0 0) (get-point leave)))
    (is (close? 1 (:time enter)))
    (is (close? 3 (:time leave))))
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point -2 -1 0) (make-vector 1 1 0)))]
    (is (equal? (make-point -1 0 0) (get-point enter)))
    (is (equal? (make-point 0 1 0) (get-point leave)))
    (is (close? 1 (:time enter)))
    (is (close? 2 (:time leave))))
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point 0 -2 0.5) (make-vector 0 1 0)))]
    (is (equal? (make-point 0 (- sqrt3_2) 0.5) (get-point enter)))
    (is (equal? (make-point 0 sqrt3_2 0.5) (get-point leave)))
    (is (close? (- 2 sqrt3_2) (:time enter)))
    (is (close? (+ 2 sqrt3_2) (:time leave))))
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point 1 -3 0) (make-vector 0 1 0)))]
    (is (equal? (make-point 1 0 0) (get-point enter)))
    (is (equal? (make-point 1 0 0) (get-point leave)))
    (is (close? 3 (:time enter)))
    (is (close? 3 (:time leave))))
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point (- (Math/sqrt 2)) 0 0) (make-vector 1 0 1)))]
    (is (equal? (make-point (- sqrt2_2) 0 sqrt2_2) (get-point enter)))
    (is (equal? (make-point (- sqrt2_2) 0 sqrt2_2) (get-point leave)))
    (is (close? sqrt2_2 (:time enter)))
    (is (close? sqrt2_2 (:time leave))))
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point (- (Math/sqrt 2)) 0 (- (Math/sqrt 2))) (make-vector 1 0 1)))]
    (is (equal? (make-point (- sqrt2_2) 0 (- sqrt2_2)) (get-point enter)))
    (is (equal? (make-point sqrt2_2 0 sqrt2_2) (get-point leave)))
    (is (close? sqrt2_2 (:time enter)))
    (is (close? (* 3 sqrt2_2) (:time leave))))
  (let [intersections (intersect-unit-sphere (make-ray (make-point 0 -2 0.5) (make-vector 0 -1 0)))]
    (is (= 0 (count intersections))))
  (let [intersections (intersect-unit-sphere (make-ray (make-point 2 -2 0) (make-vector 0 1 0)))]
    (is (= 0 (count intersections)))))
