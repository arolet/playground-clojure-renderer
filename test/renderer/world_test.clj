(ns renderer.world_test
  (:require [clojure.test :refer :all]
            [renderer.world :refer :all]
            [renderer.ray :refer [makeRay makeIntersection]]
            [renderer.light :refer [pointLight]]
            [renderer.tuple :refer [makePoint makeVector equal]]
            [renderer.color :refer [makeColor]]
            [renderer.objects :refer [makeSphere computeIntersectionState]]
            [renderer.material :refer [material]]))

(deftest testEmptyWorld
  (is (= [] (:objects (world))))
  (is (= nil (:light (world))))
  )

(deftest testAddObject
  (is (some #(= (makeSphere) %) (:objects (addObject (world) (makeSphere)))))
  (is (some #(= (makeSphere {:a 1}) %) (:objects (addObject (addObject (world) (makeSphere)) (makeSphere {:a 1})))))
  )

(defn defaultWorld
  ([] (defaultWorld (pointLight (makePoint -10 10 -10))))
  ([light] (world [(makeSphere (material (makeColor 0.8 1 0.6) 0.1 0.7 0.2))
                   (makeSphere (material) [0 0 0] [0 0 0] [0.5 0.5 0.5])]
                  light)
   )
  )

(deftest testIntersectWorld
  (let [intersections (intersect (defaultWorld) (makeRay (makePoint 0 0 -5) (makeVector 0 0 1)))]
    (is (= 4 (count intersections)))
    (is (== 4 (:time (nth intersections 0))))
    (is (== 4.5 (:time (nth intersections 1))))
    (is (== 5.5 (:time (nth intersections 2))))
    (is (== 6 (:time (nth intersections 3))))
    )
  )

(deftest testShadeHit
  (let [world (defaultWorld)
        ray (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))
        shape (nth (:objects world) 0)
        hit (makeIntersection ray 4 shape)
        state (computeIntersectionState hit)]
    (is (equal (makeColor 0.38066 0.47583 0.2855) (shadeHit world state) 1e-5)))
  (let [world (defaultWorld (pointLight (makePoint 0 0.25 0)))
        ray (makeRay (makePoint 0 0 0) (makeVector 0 0 1))
        shape (nth (:objects world) 1)
        hit (makeIntersection ray 0.5 shape)
        state (computeIntersectionState hit)]
    (is (equal (makeColor 0.90498 0.90498 0.90498) (shadeHit world state) 1e-5)))
  )

(deftest testColorAt
  (is (equal defaultBackground (colorAt (defaultWorld) (makeRay (makePoint 0 0 -5) (makeVector 0 1 0)))))
  (is (equal (makeColor 0.38066 0.47583 0.2855)
             (colorAt (defaultWorld) (makeRay (makePoint 0 0 -5) (makeVector 0 0 1))) 1e-5))
  (is (equal (makeColor 0.1 0.1 0.1)
             (colorAt (defaultWorld) (makeRay (makePoint 0 0 0.75) (makeVector 0 0 -1)))))
  )
