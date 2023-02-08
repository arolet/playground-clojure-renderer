(ns renderer.screen_test
  (:require [clojure.test :refer :all]
            [renderer.screen :refer :all]
            [renderer.utils :refer :all]
            [renderer.tuple :refer [equal makePoint makeVector]]))

(deftest testMakeScreen
  (let [screen (makeScreen 100 100 (makePoint 1 1 -1) (makePoint 1 1 1)
                           (makePoint 1 -1 -1))]
    (is (equal (makePoint 1 1 -1) (:origin screen)))
    (is (equal (makeVector 0 0 0.02) (:vX screen)))
    (is (equal (makeVector 0 -0.02 0) (:vY screen)))
    )
  (let [screen (makeScreen 50 100 (makePoint 1 1 -1) (makePoint 1 1 1)
                           (makePoint 1 -1 -1))]
    (is (equal (makePoint 1 1 -1) (:origin screen)))
    (is (equal (makeVector 0 0 0.04) (:vX screen)))
    (is (equal (makeVector 0 -0.02 0) (:vY screen)))
    )
  (let [screen (makeScreen 100 25 (makePoint 1 1 -1) (makePoint 1 1 1)
                           (makePoint 1 -1 -1))]
    (is (equal (makePoint 1 1 -1) (:origin screen)))
    (is (equal (makeVector 0 0 0.02) (:vX screen)))
    (is (equal (makeVector 0 -0.08 0) (:vY screen)))
    )
  )

(deftest testGetPixelPoint
  (let [screen (makeScreen 100 100 (makePoint 1 1 -1) (makePoint 1 1 1)
                           (makePoint 1 -1 -1))]
    (is (equal (makePoint 1 0.99 -0.99) (getPixelPoint screen 0 0)))
    (is (equal (makePoint 1 -0.01 0.01) (getPixelPoint screen 50 50)))
    (is (equal (makePoint 1 -0.99 0.99) (getPixelPoint screen 99 99)))
    )
  )
