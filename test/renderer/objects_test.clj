(ns renderer.objects_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer [mDotVec makeIdentity matEqual]]
            [renderer.objects :refer :all]
            [renderer.ray :refer [makeRay]]
            [renderer.tuple :refer [equal makePoint makeVector]]
            [renderer.utils :refer [closeTo]]))

(deftest testMakeSphere
  (is (= "sphere" (:type (makeSphere))))
  (is (matEqual (makeIdentity 4) (:transformation (makeSphere))))
  )

(deftest testIntersect
  (is (equal (makePoint 1 2 3)
             (mDotVec (:transformation (makeSphere [1 2 3])) (makePoint 0 0 0))))
  (let [ray (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))
        sphere (makeSphere [0 0 0] [0 0 0] [2 2 2])
        intersections (intersect ray [sphere])]
    (is (= 2 (count intersections)))
    (is (closeTo (:time (nth intersections 0)) 3))
    (is (closeTo (:time (nth intersections 1)) 7))
    (is (objEqual (:object3d (nth intersections 0)) sphere))
    (is (objEqual (:object3d (nth intersections 1)) sphere))
    (is (objEqual (:ray (nth intersections 0)) ray))
    (is (objEqual (:ray (nth intersections 1)) ray))
    )
  (let [ray (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))
        sphere (makeSphere [5 0 0])
        intersections (intersect ray [sphere])]
    (is (= 0 (count intersections)))
    )
  )
