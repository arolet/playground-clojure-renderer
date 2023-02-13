(ns renderer.objects.plane_test
  (:require [clojure.test :refer :all]
            [renderer.constants :refer :all]
            [renderer.material :refer
             [make-material]]
            [renderer.objects.factory :refer [make-object]]
            [renderer.objects.objects :refer :all]
            [renderer.objects.plane :refer :all]
            [renderer.ray :as Ray]
            [renderer.ray :refer [get-point make-ray]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.utils :refer [close?]]))


(deftest test-normal-on-y-plane
  (is (equal? (make-vector 0 1 0) (normal-on-y-plane (make-point 1 0 0))))
  (is (equal? (make-vector 0 1 0) (normal-on-y-plane (make-point 1 0 1))))
  (is (equal? (make-vector 0 1 0) (normal-on-y-plane (make-point 10 0 -50)))))

(deftest test-intersect-y-plane
  (is (empty? (intersect-y-plane (Ray/make-ray (make-point 0 10 0) (make-vector 0 0 1)))) "Parallel out of plane")
  (is (empty? (intersect-y-plane (Ray/make-ray (make-point 0 10 0) (make-vector 1 0 0)))) "Parallel out of plane")
  (is (empty? (intersect-y-plane (Ray/make-ray (make-point 0 0 0) (make-vector 0 0 1)))) "Parallel on plane")
  (let [intersection (intersect-y-plane (make-ray (make-point 0 1 0) (make-vector 0 -1 0)))]
    (is (= 1 (count intersection)))
    (is (close? 1 (:time (nth intersection 0)))))
  (let [intersection (intersect-y-plane (make-ray (make-point 0 -1 0) (make-vector 0 1 0)))]
    (is (= 1 (count intersection)))
    (is (close? 1 (:time (nth intersection 0)))))
  (let [intersection (intersect-y-plane (make-ray (make-point 0 -1 0) (make-vector 0 -1 0)))]
    (is (= 1 (count intersection)))
    (is (close? -1 (:time (nth intersection 0))))))

(deftest test-z-plane
  (let [plane (make-object :plane make-material [0 0 0] [(/ Math/PI 2) 0 0])]
    (is (equal? (make-vector 0 0 1) (normal-at plane (make-point 1 0 0))))
    (is (empty? (intersect (Ray/make-ray (make-point 0 0 10) (make-vector 0 1 0)) [plane])) "Parallel out of plane")
    (is (empty? (intersect (Ray/make-ray (make-point 0 0 10) (make-vector 1 0 0)) [plane])) "Parallel out of plane")
    (is (empty? (intersect (Ray/make-ray (make-point 0 0 0) (make-vector 0 1 0)) [plane])) "Parallel on plane")
    (let [intersection (intersect (make-ray (make-point 0 0 1) (make-vector 0 0 -1)) [plane])]
      (is (= 1 (count intersection)))
      (is (close? 1 (:time (nth intersection 0)))))
    (let [intersection (intersect (make-ray (make-point 0 0 -1) (make-vector 0 0 1)) [plane])]
      (is (= 1 (count intersection)))
      (is (close? 1 (:time (nth intersection 0)))))
    (let [intersection (intersect (make-ray (make-point 0 0 -1) (make-vector 0 0 -1)) [plane])]
      (is (= 1 (count intersection)))
      (is (close? -1 (:time (nth intersection 0)))))))


(deftest test-x-plane
  (let [plane (make-object :plane make-material [0 0 0] [0 0 (- (/ Math/PI 2))])]
    (is (equal? (make-vector 1 0 0) (normal-at plane (make-point 1 0 0))))
    (is (empty? (intersect (Ray/make-ray (make-point 10 0 0) (make-vector 0 1 0)) [plane])) "Parallel out of plane")
    (is (empty? (intersect (Ray/make-ray (make-point 10 0 0) (make-vector 0 0 1)) [plane])) "Parallel out of plane")
    (is (empty? (intersect (Ray/make-ray (make-point 0 0 0) (make-vector 0 1 0)) [plane])) "Parallel on plane")
    (let [intersection (intersect (make-ray (make-point 1 0 0) (make-vector -1 0 0)) [plane])]
      (is (= 1 (count intersection)))
      (is (close? 1 (:time (nth intersection 0)))))
    (let [intersection (intersect (make-ray (make-point -1 0 0) (make-vector 1 0 0)) [plane])]
      (is (= 1 (count intersection)))
      (is (close? 1 (:time (nth intersection 0)))))
    (let [intersection (intersect (make-ray (make-point -1 0 0) (make-vector -1 0 0)) [plane])]
      (is (= 1 (count intersection)))
      (is (close? -1 (:time (nth intersection 0)))))))
