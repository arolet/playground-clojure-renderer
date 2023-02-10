(ns renderer.objects_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer [mDotVec makeIdentity matEqual]]
            [renderer.transformation :refer [chain rotationZ scaling]]
            [renderer.objects :refer :all]
            [renderer.ray :refer [makeRay makeIntersection]]
            [renderer.tuple :refer [equal makePoint makeVector]]
            [renderer.utils :refer [closeTo]]
            [renderer.material :refer
             [material]]))

(deftest testMakeSphere
  (is (= "sphere" (:type (makeSphere nil))))
  (is (matEqual (makeIdentity 4) (:transformation (makeSphere nil))))
  )

(deftest testIntersect
  (is (equal (makePoint 1 2 3)
             (mDotVec (:transformation (makeSphere nil [1 2 3])) (makePoint 0 0 0))))
  (let [ray (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))
        sphere (makeSphere nil [0 0 0] [0 0 0] [2 2 2])
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
        sphere (makeSphere nil [5 0 0])
        intersections (intersect ray [sphere])]
    (is (= 0 (count intersections)))
    )
  )

(def sqrt2_2 (/ (Math/sqrt 2) 2))

(deftest testNormalAtUnitSphere
  (is (equal (makeVector 1 0 0) (normalAtUnitSphere (makePoint 1 0 0))))
  (is (equal (makeVector 0 1 0) (normalAtUnitSphere (makePoint 0 1 0))))
  (is (equal (makeVector 0 0 1) (normalAtUnitSphere (makePoint 0 0 1))))
  (is (equal (makeVector sqrt2_2 sqrt2_2 0) (normalAtUnitSphere (makePoint sqrt2_2 sqrt2_2 0))))
  )

(deftest testNormalAtSphere
  (is (equal (makeVector 0 sqrt2_2 (- sqrt2_2)) (normalAt (makeSphere nil [0 1 0]) (makePoint 0 (+ 1 sqrt2_2) (- sqrt2_2)))))
  (is (equal (makeVector 0 0.97014 -0.24254)
             (normalAt
               (makeSphereFromTransform nil (chain (rotationZ (/ Math/PI 5))
                                                   (scaling 1 0.5 1)))
               (makePoint 0 sqrt2_2 (- sqrt2_2))) 1e-5))
  )

(deftest testSphereMaterial
  (let [mater (material)
        sphere (makeSphere mater)]
    (is (= mater (:material sphere)))
    )
  )

(deftest testComputeIntersectionState
  (let [ray (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))
        sphere (makeSphere)
        intersection (makeIntersection ray 4 sphere)
        state (computeIntersectionState intersection)]
    (is (= 4 (:time state)))
    (is (= sphere (:object3d state)))
    (is (equal (makePoint 0 0 -1) (:point state)))
    (is (equal (makeVector 0 0 -1) (:eyeV state)))
    (is (equal (makeVector 0 0 -1) (:normal state)))
    )
  )

(deftest testComputeIntersectionStateInsideOutside
  (let [ray (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))
        sphere (makeSphere)
        intersection (makeIntersection ray 4 sphere)
        state (computeIntersectionState intersection)]
    (is (not (:inside state)))
    )
  (let [ray (makeRay (makePoint 0 0 0) (makeVector 0 0 1))
        sphere (makeSphere)
        intersection (makeIntersection ray 1 sphere)
        state (computeIntersectionState intersection)]
    (is (:inside state))
    (is (= 1 (:time state)))
    (is (= sphere (:object3d state)))
    (is (equal (makePoint 0 0 1) (:point state)))
    (is (equal (makeVector 0 0 -1) (:eyeV state)))
    (is (equal (makeVector 0 0 -1) (:normal state)))
    )
  )
