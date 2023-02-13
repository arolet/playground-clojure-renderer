(ns renderer.camera_test
  (:require [clojure.test :refer :all]
            [renderer.camera :refer :all]
            [renderer.color :as Color]
            [renderer.light :as Light]
            [renderer.material :as Material]
            [renderer.matrix :refer [->Mat make-identity mat-equal?]]
            [renderer.objects.factory :refer [make-object]]
            [renderer.transformation :refer [chain rotation-y scaling translation]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.utils :refer :all]
            [renderer.world :as World]
            [renderer.world_test :refer [default-world]]))

(deftest test-make-camera
  (let [h-size 160
        v-size 120
        fov (/ Math/PI 2)
        camera (make-camera h-size v-size fov)]
    (is (= h-size (:h-size camera)))
    (is (= v-size (:v-size camera)))
    (is (= fov (:fov camera)))
    (is (= (make-identity 4) (:transform camera))))
  (is (close? 0.01 (:px-size (make-camera 200 125 (/ Math/PI 2)))))
  (is (close? 0.01 (:px-size (make-camera 125 200 (/ Math/PI 2))))))

(deftest test-view-transform
  (is (mat-equal? (make-identity 4) (view-transform (make-point 0 0 0)
                                                    (make-point 0 0 -1)
                                                    (make-vector 0 1 0))
                  1e-5))
  (is (mat-equal? (scaling -1 1 -1) (view-transform (make-point 0 0 0)
                                                    (make-point 0 0 1)
                                                    (make-vector 0 1 0))
                  1e-5))
  (is (mat-equal? (translation 0 0 -8) (view-transform (make-point 0 0 8)
                                                       (make-point 0 0 0)
                                                       (make-vector 0 1 0))
                  1e-5))
  (is (mat-equal? (->Mat 4
                         4
                         [-0.50709 0.50709 0.67612 -2.36643
                          0.76772 0.60609 0.12122 -2.82843
                          -0.35857 0.59761 -0.71714 0
                          0 0 0 1])
                  (view-transform (make-point 1 3 2)
                                  (make-point 4 -2 8)
                                  (make-vector 1 1 0))
                  1e-5)))

(def sqrt2-2 (/ (Math/sqrt 2) 2))

(deftest test-ray-for-pixel
  (let [camera (make-camera 201 101 (/ Math/PI 2))
        ray (ray-for-pixel camera 100 50)]
    (is (equal? (make-point 0 0 0) (:origin ray) 1e-5))
    (is (equal? (make-vector 0 0 -1) (:direction ray) 1e-5)))
  (let [camera (make-camera 201 101 (/ Math/PI 2))
        ray (ray-for-pixel camera 0 0)]
    (is (equal? (make-point 0 0 0) (:origin ray) 1e-5))
    (is (equal? (make-vector 0.66519 0.33259 -0.66851) (:direction ray) 1e-5)))
  (let [camera (make-camera
                 201
                 101
                 (/ Math/PI 2)
                 (chain (translation 0 -2 5) (rotation-y (/ Math/PI 4))))
        ray (ray-for-pixel camera 100 50)]
    (is (equal? (make-point 0 2 -5) (:origin ray) 1e-5))
    (is (equal? (make-vector sqrt2-2 0 (- sqrt2-2)) (:direction ray) 1e-5))))

(deftest test-pixel-at
  (let [from (make-point 0 0 -5)
        to (make-point 0 0 0)
        up (make-vector 0 1 0)
        cam (make-camera 11 11 (/ Math/PI 2) (view-transform from to up))
        world (default-world)]
    (is (equal? (Color/make-color 0.38066 0.47583 0.2855) (pixel-at world cam 5 5) 1e-5))))

(deftest test-render
  (let [from (make-point 0 0 -5)
        to (make-point 0 0 0)
        up (make-vector 0 1 0)
        cam (make-camera 11 11 (/ Math/PI 2) (view-transform from to up))
        world (default-world)
        canvas (render world cam)]
    (is (equal? (Color/make-color 0.38066 0.47583 0.2855) (nth (nth (:data canvas) 5) 5) 1e-5))))

(def no-lighting-world (World/make-world
                         [(make-object :sphere (Material/make-material (Color/make-color 1 0 0) 1 0 0 0)
                                               [0 0 -1]
                                               [0 0 0]
                                               [100 0.65 0.5])
                          (make-object :sphere (Material/make-material (Color/make-color 0 1 0) 1 0 0 0)
                                               [0 0 -1]
                                               [0 0 0]
                                               [0.65 100 0.499])]
                         (Light/->PointLight (Color/make-color 1 1 1) (make-point 0 0 0))
                         (Color/make-color 0 0 0)))

(deftest test-render-antialias
  (let [cam (make-camera 21 21 (/ Math/PI 2))
        world no-lighting-world
        canvas (render world cam nil true)]
    (is (equal? (Color/make-color 1 0 0) (pixel-at world cam 10 10.5) 1e-5))
    (is (equal? (Color/make-color 0 1 0) (pixel-at world cam 10 11.5) 1e-5))
    (is (equal? (Color/make-color 0 1 0) (pixel-at world cam 10.5 11) 1e-5))
    (is (equal? (Color/make-color 0 1 0) (pixel-at world cam 9.5 11) 1e-5))
    (is (equal? (Color/make-color 0.5 0.5 0) (nth (nth (:data canvas) 11) 10) 1e-5))
    (is (equal? (Color/make-color 0.75 0.25 0) (nth (nth (:data canvas) 11) 11) 1e-5))))
