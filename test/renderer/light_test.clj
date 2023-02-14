(ns renderer.light_test
  (:require [clojure.test :refer :all]
            [renderer.light :refer :all]
            [renderer.texture.color :refer [make-color]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.material :refer [make-material]]
            [renderer.objects.factory :refer [make-object]]
            [renderer.ray :refer [make-ray make-intersection]]
            [renderer.world_test :refer [default-world]]))

(deftest test-make-point-light
  (is (equal? (make-color 1 1 1) (:intensity (make-point-light (make-point 1 2 3)))))
  (is (equal? (make-point 1 2 3) (:position (make-point-light (make-point 1 2 3)))))
  )

(def mater (make-material))
(def position (make-point 0 0 0))


(deftest test-compute-intersection-state
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-object :sphere)
        intersection (make-intersection ray 4 sphere)
        state (compute-intersection-state intersection (default-world))]
    (is (= 4 (:time state)))
    (is (= sphere (:object3d state)))
    (is (equal? (make-point 0 0 -1) (:point state)))
    (is (equal? (make-vector 0 0 -1) (:eyeV state)))
    (is (equal? (make-vector 0 0 -1) (:normal state)))))

(deftest test-compute-intersection-state-acne-adjusted
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-object :sphere)
        intersection (make-intersection ray 4 sphere)
        state (compute-intersection-state intersection (default-world))]
    (is (= (- -1 acne-epsilon) (nth (:point state) 2)))))

(deftest test-compute-intersection-state-inside-outside
  (let [ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        sphere (make-object :sphere)
        intersection (make-intersection ray 4 sphere)
        state (compute-intersection-state intersection (default-world))]
    (is (not (:inside state)))
    )
  (let [ray (make-ray (make-point 0 0 0) (make-vector 0 0 1))
        sphere (make-object :sphere)
        intersection (make-intersection ray 1 sphere)
        state (compute-intersection-state intersection (default-world))]
    (is (:inside state))
    (is (= 1 (:time state)))
    (is (= sphere (:object3d state)))
    (is (equal? (make-point 0 0 1) (:point state)))
    (is (equal? (make-vector 0 0 -1) (:eyeV state)))
    (is (equal? (make-vector 0 0 -1) (:normal state)))))

(deftest test-effective-color
  (is (equal? (make-color 1 1 1) (effective-color
                                 (make-point-light (make-point 0 0 -10) (make-color 1 1 1))
                                 mater)))
  (is (equal? (make-color 1 0.9 0.7) (effective-color
                                     (make-point-light (make-point 0 0 -10) (make-color 1 0.9 0.7))
                                     mater))))

(deftest test-phong-ambient
  (is (equal? (make-color 0.1 0.1 0.1) (phong-ambient
                                       (make-color 1 1 1)
                                       mater)))
  (is (equal? (make-color 0.1 0.09 0.07) (phong-ambient
                                         (make-color 1 0.9 0.7)
                                         mater))))

(deftest test-phong-diffuse
  (is (equal? (make-color 0.9 0.9 0.9) (phong-diffuse
                                       (make-color 1 1 1)
                                       mater
                                       1.)))
  (is (equal? (make-color 0.09 0.09 0.09) (phong-diffuse
                                       (make-color 1 1 1)
                                       mater
                                       0.1)))
  (is (equal? (make-color 0.09 0.081 0.063) (phong-diffuse
                                         (make-color 1 0.9 0.7)
                                         mater
                                         0.1))))

(def sqrt2_2 (/ 1 (Math/sqrt 2)))

(deftest test-phong-lighting
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 -10) (make-color 1 1 1))]
    (is (equal? (make-color 1.9 1.9 1.9)
                (phong-lighting light mater position toEye normal))))
  (let [toEye (make-vector 0 sqrt2_2 (- sqrt2_2))
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 -10) (make-color 1 1 1))]
    (is (equal? (make-color 1 1 1)
                (phong-lighting light mater position toEye normal))
         1e-4))
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 10 -10) (make-color 1 1 1))]
    (is (equal? (make-color 0.7364 0.7364 0.7364)
                (phong-lighting light mater position toEye normal)
                1e-4)))
  (let [toEye (make-vector 0 (- sqrt2_2) (- sqrt2_2))
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 10 -10) (make-color 1 1 1))]
    (is (equal? (make-color 1.6364 1.6364 1.6364)
                (phong-lighting light mater position toEye normal)
                1e-4)))
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 10) (make-color 1 1 1))]
    (is (equal? (make-color 0.1 0.1 0.1)
                (phong-lighting light mater position toEye normal)
                1e-4))))

(deftest test-phong-lighting-in-shadow
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 -10) (make-color 1 1 1))]
  (is (equal? (make-color 0.1 0.1 0.1) (phong-lighting light mater position toEye normal true)))))

(deftest test-shadowed?
  (let [world (default-world)
        light (:light world)
        objects (:objects world)]
    (is (not (shadowed? light objects (make-point 0 10 0))))
    (is (not (shadowed? light objects (make-point -20 20 20))))
    (is (not (shadowed? light objects (make-point -2 2 2))))
    (is (shadowed? light objects (make-point 10 -10 10)))))
