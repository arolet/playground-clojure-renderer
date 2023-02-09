(ns renderer.light_test
  (:require [clojure.test :refer :all]
            [renderer.light :refer :all]
            [renderer.color :refer [makeColor]]
            [renderer.tuple :refer [equal makePoint makeVector]]
            [renderer.material :refer [material]]))

(deftest testPointLight
  (is (equal (makeColor 1 1 1) (:intensity (pointLight (makePoint 1 2 3)))))
  (is (equal (makePoint 1 2 3) (:position (pointLight (makePoint 1 2 3)))))
  )

(def mater (material))
(def position (makePoint 0 0 0))

(deftest testEffectiveColor
  (is (equal (makeColor 1 1 1) (effectiveColor
                                 (pointLight (makePoint 0 0 -10) (makeColor 1 1 1))
                                 mater)))
  (is (equal (makeColor 1 0.9 0.7) (effectiveColor
                                     (pointLight (makePoint 0 0 -10) (makeColor 1 0.9 0.7))
                                     mater)))
  )

(deftest testPhongAmbient
  (is (equal (makeColor 0.1 0.1 0.1) (phongAmbient
                                       (makeColor 1 1 1)
                                       mater)))
  (is (equal (makeColor 0.1 0.09 0.07) (phongAmbient
                                         (makeColor 1 0.9 0.7)
                                         mater)))
  )

(deftest testPhongDiffuse
  (is (equal (makeColor 0.9 0.9 0.9) (phongDiffuse
                                       (makeColor 1 1 1)
                                       mater
                                       1.)
             ))
  (is (equal (makeColor 0.09 0.09 0.09) (phongDiffuse
                                       (makeColor 1 1 1)
                                       mater
                                       0.1)
             ))
  (is (equal (makeColor 0.09 0.081 0.063) (phongDiffuse
                                         (makeColor 1 0.9 0.7)
                                         mater
                                         0.1)))
  )

(def sqrt2_2 (/ 1 (Math/sqrt 2)))

(deftest testPhongLighting
  (let [toEye (makeVector 0 0 -1)
        normal (makeVector 0 0 -1)
        light (pointLight (makePoint 0 0 -10) (makeColor 1 1 1))]
    (is (equal (makeColor 1.9 1.9 1.9)
               (phongLighting light mater position toEye normal))))
  (let [toEye (makeVector 0 sqrt2_2 (- sqrt2_2))
        normal (makeVector 0 0 -1)
        light (pointLight (makePoint 0 0 -10) (makeColor 1 1 1))]
    (is (equal (makeColor 1 1 1)
               (phongLighting light mater position toEye normal))
         1e-4
        ))
  (let [toEye (makeVector 0 0 -1)
        normal (makeVector 0 0 -1)
        light (pointLight (makePoint 0 10 -10) (makeColor 1 1 1))]
    (is (equal (makeColor 0.7364 0.7364 0.7364 )
               (phongLighting light mater position toEye normal)
               1e-4)
        ))
  (let [toEye (makeVector 0 (- sqrt2_2) (- sqrt2_2))
        normal (makeVector 0 0 -1)
        light (pointLight (makePoint 0 10 -10) (makeColor 1 1 1))]
    (is (equal (makeColor 1.6364 1.6364 1.6364)
               (phongLighting light mater position toEye normal)
               1e-4)
        ))
  (let [toEye (makeVector 0 0 -1)
        normal (makeVector 0 0 -1)
        light (pointLight (makePoint 0 0 10) (makeColor 1 1 1))]
    (is (equal (makeColor 0.1 0.1 0.1)
               (phongLighting light mater position toEye normal)
               1e-4)
        ))
  )
