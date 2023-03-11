(ns renderer.texture.stripes_test
  (:require [clojure.test :refer :all]
            [renderer.texture.color :refer [equal? make-color]]
            [renderer.texture.stripes :refer :all]
            [renderer.texture.texture :refer [sample-texture]]
            [renderer.transformation :refer [rotation-xyz]]
            [renderer.tuple :refer [make-point]]))

(deftest test-default-stripes
  (let [text (stripes)]
  (is (equal? (make-color 1 1 1) (sample-texture text (make-point 0 0 0))))
  (is (equal? (make-color 0 0 0) (sample-texture text (make-point 1 0 0))))
  (is (equal? (make-color 1 1 1) (sample-texture text (make-point 0 1 0))))
  (is (equal? (make-color 0 0 0) (sample-texture text (make-point 1 2 1))))
  (is (equal? (make-color 1 1 1) (sample-texture text (make-point 0 3 2))))
  (is (equal? (make-color 0 0 0) (sample-texture text (make-point 1 4 3))))
  (is (equal? (make-color 0 0 0) (sample-texture text (make-point -0.5 0 0))))
  (is (equal? (make-color 0 0 0) (sample-texture text (make-point -1 4 3))))
  (is (equal? (make-color 1 1 1) (sample-texture text (make-point -1.5 4 3))))))

(deftest test-stripes
  (let [transform (rotation-xyz 0 0 0)
        on (make-color 1 0 0)
        off (make-color 0 0 1)
        text (stripes transform false on off)]
    (is (equal? (make-color 1 0 0) (sample-texture text (make-point 0 0 0))))
    (is (equal? (make-color 0 0 1) (sample-texture text (make-point 1 0 0)))))
  (let [transform (rotation-xyz 0 0 (/ Math/PI 2))
        on (make-color 1 0 0)
        off (make-color 0 0 1)
        text (stripes transform false on off)]
    (is (equal? (make-color 1 0 0) (sample-texture text (make-point 0 0 0))))
    (is (equal? (make-color 0 0 1) (sample-texture text (make-point 0 1 0))))))

(deftest test-checker
  (let [text (checker)]
    (is (equal? (make-color 1 1 1) (sample-texture text (make-point 0 0 0))))
    (is (equal? (make-color 0 0 0) (sample-texture text (make-point 1 0 0))))
    (is (equal? (make-color 1 1 1) (sample-texture text (make-point 1 0 1))))
    ))
