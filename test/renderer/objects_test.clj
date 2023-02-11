(ns renderer.objects_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer [m-dot-vec make-identity mat-equal?]]
            [renderer.transformation :refer [chain rotation-z scaling]]
            [renderer.objects :refer :all]
            [renderer.ray :refer [make-ray make-intersection get-point]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.utils :refer [close?]]
            [renderer.material :refer
             [make-material]]))

(deftest test-make-sphere
  (is (= "sphere" (:type (make-sphere nil))))
  (is (mat-equal? (make-identity 4) (:transformation (make-sphere nil))))
  )

(deftest test-intersect
  (is (equal? (make-point 1 2 3)
              (m-dot-vec (:transformation (make-sphere nil [1 2 3])) (make-point 0 0 0))))
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-sphere nil [0 0 0] [0 0 0] [2 2 2])
        intersections (intersect ray [sphere])]
    (is (= 2 (count intersections)))
    (is (close? (:time (nth intersections 0)) 3))
    (is (close? (:time (nth intersections 1)) 7))
    (is (objEqual (:object3d (nth intersections 0)) sphere))
    (is (objEqual (:object3d (nth intersections 1)) sphere))
    (is (objEqual (:ray (nth intersections 0)) ray))
    (is (objEqual (:ray (nth intersections 1)) ray))
    )
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-sphere nil [5 0 0])
        intersections (intersect ray [sphere])]
    (is (= 0 (count intersections)))
    )
  )

(def sqrt2_2 (/ (Math/sqrt 2) 2))

(deftest test-normal-at-unit-sphere
  (is (equal? (make-vector 1 0 0) (normal-at-unit-sphere (make-point 1 0 0))))
  (is (equal? (make-vector 0 1 0) (normal-at-unit-sphere (make-point 0 1 0))))
  (is (equal? (make-vector 0 0 1) (normal-at-unit-sphere (make-point 0 0 1))))
  (is (equal? (make-vector sqrt2_2 sqrt2_2 0) (normal-at-unit-sphere (make-point sqrt2_2 sqrt2_2 0))))
  )

(deftest test-normal-at-sphere
  (is (equal? (make-vector 0 sqrt2_2 (- sqrt2_2)) (normal-at (make-sphere nil [0 1 0]) (make-point 0 (+ 1 sqrt2_2) (- sqrt2_2)))))
  (is (equal? (make-vector 0 0.97014 -0.24254)
              (normal-at
               (make-sphere-from-transform nil (chain (rotation-z (/ Math/PI 5))
                                                      (scaling 1 0.5 1)))
               (make-point 0 sqrt2_2 (- sqrt2_2))) 1e-5))
  )

(deftest test-sphere-material
  (let [mater (make-material)
        sphere (make-sphere mater)]
    (is (= mater (:material sphere)))
    )
  )

(deftest test-compute-intersection-state
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-sphere)
        intersection (make-intersection ray 4 sphere)
        state (compute-intersection-state intersection)]
    (is (= 4 (:time state)))
    (is (= sphere (:object3d state)))
    (is (equal? (make-point 0 0 -1) (:point state)))
    (is (equal? (make-vector 0 0 -1) (:eyeV state)))
    (is (equal? (make-vector 0 0 -1) (:normal state)))
    )
  )

(deftest test-compute-intersection-state-inside-outside
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-sphere)
        intersection (make-intersection ray 4 sphere)
        state (compute-intersection-state intersection)]
    (is (not (:inside state)))
    )
  (let [ray (make-ray (make-point 0 0 0) (make-vector 0 0 1))
        sphere (make-sphere)
        intersection (make-intersection ray 1 sphere)
        state (compute-intersection-state intersection)]
    (is (:inside state))
    (is (= 1 (:time state)))
    (is (= sphere (:object3d state)))
    (is (equal? (make-point 0 0 1) (:point state)))
    (is (equal? (make-vector 0 0 -1) (:eyeV state)))
    (is (equal? (make-vector 0 0 -1) (:normal state)))
    )
  )


(def sqrt3_2 (/ (Math/sqrt 3) 2))

(deftest test-intersect-unit-sphere
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point -2 0 0) (make-vector 1 0 0)))]
    (is (equal? (make-point -1 0 0) (get-point enter)))
    (is (equal? (make-point 1 0 0) (get-point leave)))
    (is (close? 1 (:time enter)))
    (is (close? 3 (:time leave)))
    )
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point -2 -1 0) (make-vector 1 1 0)))]
    (is (equal? (make-point -1 0 0) (get-point enter)))
    (is (equal? (make-point 0 1 0) (get-point leave)))
    (is (close? 1 (:time enter)))
    (is (close? 2 (:time leave)))
    )
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point 0 -2 0.5) (make-vector 0 1 0)))]
    (is (equal? (make-point 0 (- sqrt3_2) 0.5) (get-point enter)))
    (is (equal? (make-point 0 sqrt3_2 0.5) (get-point leave)))
    (is (close? (- 2 sqrt3_2) (:time enter)))
    (is (close? (+ 2 sqrt3_2) (:time leave)))
    )
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point 1 -3 0) (make-vector 0 1 0)))]
    (is (equal? (make-point 1 0 0) (get-point enter)))
    (is (equal? (make-point 1 0 0) (get-point leave)))
    (is (close? 3 (:time enter)))
    (is (close? 3 (:time leave)))
    )
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point (- (Math/sqrt 2)) 0 0) (make-vector 1 0 1)))]
    (is (equal? (make-point (- sqrt2_2) 0 sqrt2_2) (get-point enter)))
    (is (equal? (make-point (- sqrt2_2) 0 sqrt2_2) (get-point leave)))
    (is (close? sqrt2_2 (:time enter)))
    (is (close? sqrt2_2 (:time leave)))
    )
  (let [[enter leave] (intersect-unit-sphere (make-ray (make-point (- (Math/sqrt 2)) 0 (- (Math/sqrt 2))) (make-vector 1 0 1)))]
    (is (equal? (make-point (- sqrt2_2) 0 (- sqrt2_2)) (get-point enter)))
    (is (equal? (make-point sqrt2_2 0 sqrt2_2) (get-point leave)))
    (is (close? sqrt2_2 (:time enter)))
    (is (close? (* 3 sqrt2_2) (:time leave)))
    )
  (let [intersections (intersect-unit-sphere (make-ray (make-point 0 -2 0.5) (make-vector 0 -1 0)))]
    (is (= 0 (count intersections)))
    )
  (let [intersections (intersect-unit-sphere (make-ray (make-point 2 -2 0) (make-vector 0 1 0)))]
    (is (= 0 (count intersections)))
    )
  )
