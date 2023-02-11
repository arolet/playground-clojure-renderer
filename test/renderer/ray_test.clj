(ns renderer.ray_test
  (:require [clojure.test :refer :all]
            [renderer.ray :refer :all]
            [renderer.tuple :refer
             [equal? point? tuple-vector?
              make-point make-vector norm]]
            [renderer.utils :refer [close?]]))


(defn assert-ray [ray]
  (is (point? (:origin ray)) (format "Origin (%s) is a point" (:origin ray)))
  (is (tuple-vector? (:direction ray)) (format "Direction (%s) is a vector" (:direction ray)))
  )

(deftest test-make-ray
  (let [testFn (fn [origin direction]
                 (let [ray (make-ray origin direction)]
                   (assert-ray ray)
                   (is (equal? origin (:origin ray)))
                   (is (equal? direction (:direction ray)))
                   (is (close? (norm direction) (:norm ray)))
                   )
                 )
        ]
    (testFn (make-point 0 0 0) (make-vector 1 0 0))
    (testFn (make-point 0 1 0) (make-vector 1 1 0))
    (testFn (make-point 0 0 3) (make-vector 1 1 -1))
    )
  )

(def sqrt2_2 (/ (Math/sqrt 2) 2))

(deftest test-project-point
  (let [point (make-point 0 0 0)
        ray (make-ray (make-point 1 1 0) (make-vector 0 -1 0))
        [proj dist t] (project-point ray point)]
    (is (equal? proj (make-point 1 0 0)))
    (is (== dist 1))
    (is (== t 1))
    )
  (let [point (make-point 0 0 0)
        ray (make-ray (make-point 1 1 0) (make-vector 0 1 0))
        [proj dist t] (project-point ray point)]
    (is (equal? proj (make-point 1 0 0)))
    (is (== dist 1))
    (is (== t (- 1)))
    )
  (let [point (make-point 0 0 0)
        ray (make-ray (make-point -1 0 0) (make-vector 1 1 0))
        [proj dist t] (project-point ray point)]
    (is (equal? proj (make-point -0.5 0.5 0)))
    (is (close? dist sqrt2_2))
    (is (close? t 0.5))
    )
  )

(deftest test-hit
  (let [i1 (->Intersection nil 1 nil)
        i2 (->Intersection nil 2 nil)]
    (is (= i1 (hit [i1 i2]))))
  (let [i1 (->Intersection nil -1 nil)
        i2 (->Intersection nil 2 nil)]
    (is (= i2 (hit [i1 i2]))))
  (let [i1 (->Intersection nil -1 nil)
        i2 (->Intersection nil -2 nil)]
    (is (= nil (hit [i1 i2]))))
  )
