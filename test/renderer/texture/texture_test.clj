(ns renderer.texture.texture_test
  (:require [clojure.test :refer :all]
            [renderer.objects.factory :refer [make-object]]
            [renderer.texture.color :refer [equal? make-color]]
            [renderer.texture.stripes :refer :all]
            [renderer.texture.texture :refer :all]
            [renderer.transformation :refer [rotation-xyz]]
            [renderer.tuple :refer [make-point]]))

(deftest test-sample-texture
  (let [text (stripes)
        obj (make-object :plane {} [0 0 0] [0 (/ Math/PI 2) 0] [0.5 0.5 0.5])]
    (is (equal? (make-color 1 1 1) (sample-texture text obj (make-point 0 0 0))))
    (is (equal? (make-color 0 0 0) (sample-texture text obj (make-point 1 0 0))))
    (is (equal? (make-color 1 1 1) (sample-texture text obj (make-point -1.5 4 3)))))
  (let [text (stripes (rotation-xyz 0 0 0) true)
        obj (make-object :plane {} [0 0 0] [0 (/ Math/PI 2) 0] [0.5 0.5 0.5])]
    (is (equal? (make-color 1 1 1) (sample-texture text obj (make-point 0 0 0))))
    (is (equal? (make-color 0 0 0) (sample-texture text obj (make-point 0 0 0.5))))
    (is (equal? (make-color 0 0 0) (sample-texture text obj (make-point 0 0 0.3))))
    (is (equal? (make-color 1 1 1) (sample-texture text obj (make-point 0 0 0.6))))
    (is (equal? (make-color 1 1 1) (sample-texture text obj (make-point 0 0 1))))))

