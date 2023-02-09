(ns renderer.ray_test
  (:require [clojure.test :refer :all]
            [renderer.ray :refer :all]
            [renderer.tuple :refer
             [equal isPoint isVector
              makePoint makeVector norm]]
            [renderer.utils :refer [closeTo]]))


(defn assertCorrectRay [ray]
  (is (isPoint (:origin ray)) (format "Origin (%s) is a point" (:origin ray)))
  (is (isVector (:direction ray)) (format "Direction (%s) is a vector" (:direction ray)))
  )

(deftest testMakeRay
  (let [testFn (fn [origin direction]
                 (let [ray (makeRay origin direction)]
                   (assertCorrectRay ray)
                   (is (equal origin (:origin ray)))
                   (is (equal direction (:direction ray)))
                   (is (closeTo (norm direction) (:norm ray)))
                   )
                 )
        ]
    (testFn (makePoint 0 0 0) (makeVector 1 0 0))
    (testFn (makePoint 0 1 0) (makeVector 1 1 0))
    (testFn (makePoint 0 0 3) (makeVector 1 1 -1))
    )
  )

(def sqrt2_2 (/ (Math/sqrt 2) 2))

(deftest testProjectPoint
  (let [point (makePoint 0 0 0)
        ray (makeRay (makePoint 1 1 0) (makeVector 0 -1 0))
        [proj dist t] (projectPoint ray point)]
    (is (equal proj (makePoint 1 0 0)))
    (is (== dist 1))
    (is (== t 1))
    )
  (let [point (makePoint 0 0 0)
        ray (makeRay (makePoint 1 1 0) (makeVector 0 1 0))
        [proj dist t] (projectPoint ray point)]
    (is (equal proj (makePoint 1 0 0)))
    (is (== dist 1))
    (is (== t (- 1)))
    )
  (let [point (makePoint 0 0 0)
        ray (makeRay (makePoint -1 0 0) (makeVector 1 1 0))
        [proj dist t] (projectPoint ray point)]
    (is (equal proj (makePoint -0.5 0.5 0)))
    (is (closeTo dist sqrt2_2))
    (is (closeTo t 0.5))
    )
  )


(def sqrt3_2 (/ (Math/sqrt 3) 2))

(deftest testIntersectUnitSphere
  (let [[enter leave] (intersectUnitSphere (makeRay (makePoint -2 0 0) (makeVector 1 0 0)))]
    (is (equal (makePoint -1 0 0) (getPoint enter)))
    (is (equal (makePoint 1 0 0) (getPoint leave)))
    (is (closeTo 1 (:time enter)))
    (is (closeTo 3 (:time leave)))
    )
  (let [[enter leave] (intersectUnitSphere (makeRay (makePoint -2 -1 0) (makeVector 1 1 0)))]
    (is (equal (makePoint -1 0 0) (getPoint enter)))
    (is (equal (makePoint 0 1 0) (getPoint leave)))
    (is (closeTo 1 (:time enter)))
    (is (closeTo 2 (:time leave)))
    )
  (let [[enter leave] (intersectUnitSphere (makeRay (makePoint 0 -2 0.5) (makeVector 0 1 0)))]
    (is (equal (makePoint 0 (- sqrt3_2) 0.5) (getPoint enter)))
    (is (equal (makePoint 0 sqrt3_2 0.5) (getPoint leave)))
    (is (closeTo (- 2 sqrt3_2) (:time enter)))
    (is (closeTo (+ 2 sqrt3_2) (:time leave)))
    )
  (let [[enter leave] (intersectUnitSphere (makeRay (makePoint 1 -3 0) (makeVector 0 1 0)))]
    (is (equal (makePoint 1 0 0) (getPoint enter)))
    (is (equal (makePoint 1 0 0) (getPoint leave)))
    (is (closeTo 3 (:time enter)))
    (is (closeTo 3 (:time leave)))
    )
  (let [[enter leave] (intersectUnitSphere (makeRay (makePoint (- (Math/sqrt 2)) 0 0) (makeVector 1 0 1)))]
    (is (equal (makePoint (- sqrt2_2) 0 sqrt2_2) (getPoint enter)))
    (is (equal (makePoint (- sqrt2_2) 0 sqrt2_2) (getPoint leave)))
    (is (closeTo sqrt2_2 (:time enter)))
    (is (closeTo sqrt2_2 (:time leave)))
    )
  (let [[enter leave] (intersectUnitSphere (makeRay (makePoint (- (Math/sqrt 2)) 0 (- (Math/sqrt 2))) (makeVector 1 0 1)))]
    (is (equal (makePoint (- sqrt2_2) 0 (- sqrt2_2)) (getPoint enter)))
    (is (equal (makePoint sqrt2_2 0 sqrt2_2) (getPoint leave)))
    (is (closeTo sqrt2_2 (:time enter)))
    (is (closeTo (* 3 sqrt2_2) (:time leave)))
    )
  (let [intersections (intersectUnitSphere (makeRay (makePoint 0 -2 0.5) (makeVector 0 -1 0)))]
    (is (= 0 (count intersections)))
    )
  (let [intersections (intersectUnitSphere (makeRay (makePoint 2 -2 0) (makeVector 0 1 0)))]
    (is (= 0 (count intersections)))
    )
  )

(deftest testHit
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
