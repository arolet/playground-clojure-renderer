(ns renderer.world_test
  (:require [clojure.test :refer :all]
            [renderer.light :refer [compute-intersection-state make-point-light]]
            [renderer.material :refer [make-material]]
            [renderer.matrix :refer [make-identity]]
            [renderer.objects.factory :refer [make-object]]
            [renderer.ray :refer [make-intersection make-ray]]
            [renderer.texture.color :refer [make-color]]
            [renderer.texture.stripes :refer [stripes]]
            [renderer.texture.uniform :refer [uniform-texture]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.world :refer :all]))

(deftest test-empty-world
  (is (= [] (:objects (make-world))))
  (is (= nil (:light (make-world)))))

(deftest test-add-object
  (let [obj (make-object :sphere)]
    (is (some #(= obj %) (:objects (add-object (make-world) obj)))))
  (let [obj (make-object :sphere {:a 1})]
    (is (some #(= obj %) (:objects (add-object (add-object (make-world) (make-object :sphere)) obj))))))

(defn default-world
  ([] (default-world (make-point-light (make-point -10 10 -10))))
  ([light] (make-world [(make-object :sphere (make-material (uniform-texture (make-color 0.8 1 0.6)) 0.1 0.7 0.2))
                        (make-object :sphere (make-material) [0 0 0] [0 0 0] [0.5 0.5 0.5])]
                       light)))

(deftest test-intersect-world
  (let [intersections (intersect (default-world) (make-ray (make-point 0 0 -5) (make-vector 0 0 1)))]
    (is (= 4 (count intersections)))
    (is (== 4 (:time (nth intersections 0))))
    (is (== 4.5 (:time (nth intersections 1))))
    (is (== 5.5 (:time (nth intersections 2))))
    (is (== 6 (:time (nth intersections 3))))))

(deftest test-shade-hit
  (let [world (default-world)
        ray (make-ray (make-point 0 0 -5) (make-vector 0 0 1))
        shape (nth (:objects world) 0)
        hit (make-intersection ray 4 shape)
        state (compute-intersection-state hit world)]
    (is (equal? (make-color 0.38066 0.47583 0.2855) (shade-hit world state) 1e-5)))
  (let [world (default-world (make-point-light (make-point 0 0.25 0)))
        ray (make-ray (make-point 0 0 0) (make-vector 0 0 1))
        shape (nth (:objects world) 1)
        hit (make-intersection ray 0.5 shape)
        state (compute-intersection-state hit world)]
    (is (equal? (make-color 0.90498 0.90498 0.90498) (shade-hit world state) 1e-5))))

(deftest test-shade-hit-in-shadow
  (let [s1 (make-object :sphere (make-material))
        s2 (make-object :sphere (make-material) [0 0 10])
        world (make-world [s1 s2] (make-point-light (make-point 0 0 -10)))
        ray (make-ray (make-point 0 0 5) (make-vector 0 0 1))
        hit (make-intersection ray 4 s2)
        state (compute-intersection-state hit world)]
    (is (equal? (make-color 0.1 0.1 0.1) (shade-hit world state) 1e-5))))

(deftest test-color-at
  (is (equal? default-background (color-at (default-world) (make-ray (make-point 0 0 -5) (make-vector 0 1 0)))))
  (is (equal? (make-color 0.38066 0.47583 0.2855)
              (color-at (default-world) (make-ray (make-point 0 0 -5) (make-vector 0 0 1))) 1e-5))
  (is (equal? (make-color 0.1 0.1 0.1)
              (color-at (default-world) (make-ray (make-point 0 0 0.75) (make-vector 0 0 -1))))))

(deftest test-color-at-striped-plane
  (let [on (make-color 1 0 0)
        off (make-color 0 1 0)
        texture (stripes (make-identity 4) true on off)
        plane (make-object :plane (make-material texture 1 0 0 0))
        world (make-world [plane] (make-point-light (make-point 0 10 0)))
        eye (make-point 0 1 0)]
    (is (equal? on (color-at world (make-ray eye (make-vector 0 -1 0) true))))
    (is (equal? off (color-at world (make-ray eye (make-vector 1 -1 0) true))))
    (is (equal? off (color-at world (make-ray eye (make-vector 1.1 -1 0) true))))
    ))
