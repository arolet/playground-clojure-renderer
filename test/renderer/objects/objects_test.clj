(ns renderer.objects.objects_test
  (:require [clojure.test :refer :all]
            [renderer.constants :refer :all]
            [renderer.matrix :refer [m-dot-vec]]
            [renderer.objects.objects :refer :all]
            [renderer.objects.factory :refer [make-object make-object-from-transform]]
            [renderer.ray :refer [make-ray]]
            [renderer.transformation :refer [chain rotation-z scaling]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.utils :refer [close?]]))

(deftest test-intersect
  (is (equal? (make-point 1 2 3)
              (m-dot-vec (get-transformation (make-object :sphere nil [1 2 3])) (make-point 0 0 0))))
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-object :sphere nil [0 0 0] [0 0 0] [2 2 2])
        intersections (intersect ray [sphere])]
    (is (= 2 (count intersections)))
    (is (close? (:time (nth intersections 0)) 3))
    (is (close? (:time (nth intersections 1)) 7))
    (is (objEqual (:object3d (nth intersections 0)) sphere))
    (is (objEqual (:object3d (nth intersections 1)) sphere))
    (is (objEqual (:ray (nth intersections 0)) ray))
    (is (objEqual (:ray (nth intersections 1)) ray)))
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-object :sphere nil [5 0 0])
        intersections (intersect ray [sphere])]
    (is (= 0 (count intersections)))))

(deftest test-normal-at-sphere
  (is (equal? (make-vector 0 sqrt2_2 (- sqrt2_2)) (normal-at (make-object :sphere nil [0 1 0]) (make-point 0 (+ 1 sqrt2_2) (- sqrt2_2)))))
  (is (equal? (make-vector 0 0.97014 -0.24254)
              (normal-at
                (make-object-from-transform :sphere
                                            (chain (rotation-z (/ Math/PI 5))
                                                       (scaling 1 0.5 1))
                                            nil)
                (make-point 0 sqrt2_2 (- sqrt2_2))) 1e-5)))
