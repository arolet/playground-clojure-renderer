(ns renderer.light_test
  (:require [clojure.test :refer :all]
            [renderer.light :refer :all]
            [renderer.color :refer [make-color]]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.material :refer [make-material]]))

(deftest test-make-point-ight
  (is (equal? (make-color 1 1 1) (:intensity (make-point-light (make-point 1 2 3)))))
  (is (equal? (make-point 1 2 3) (:position (make-point-light (make-point 1 2 3)))))
  )

(def mater (make-material))
(def position (make-point 0 0 0))

(deftest test-effective-color
  (is (equal? (make-color 1 1 1) (effective-color
                                 (make-point-light (make-point 0 0 -10) (make-color 1 1 1))
                                 mater)))
  (is (equal? (make-color 1 0.9 0.7) (effective-color
                                     (make-point-light (make-point 0 0 -10) (make-color 1 0.9 0.7))
                                     mater)))
  )

(deftest test-phong-ambient
  (is (equal? (make-color 0.1 0.1 0.1) (phong-ambient
                                       (make-color 1 1 1)
                                       mater)))
  (is (equal? (make-color 0.1 0.09 0.07) (phong-ambient
                                         (make-color 1 0.9 0.7)
                                         mater)))
  )

(deftest test-phong-diffuse
  (is (equal? (make-color 0.9 0.9 0.9) (phong-diffuse
                                       (make-color 1 1 1)
                                       mater
                                       1.)
              ))
  (is (equal? (make-color 0.09 0.09 0.09) (phong-diffuse
                                       (make-color 1 1 1)
                                       mater
                                       0.1)
              ))
  (is (equal? (make-color 0.09 0.081 0.063) (phong-diffuse
                                         (make-color 1 0.9 0.7)
                                         mater
                                         0.1)))
  )

(def sqrt2_2 (/ 1 (Math/sqrt 2)))

(deftest test-make-point-light
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 -10) (make-color 1 1 1))]
    (is (equal? (make-color 1.9 1.9 1.9)
                (phong-lighting light mater position toEye normal))))
  (let [toEye (make-vector 0 sqrt2_2 (- sqrt2_2))
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 -10) (make-color 1 1 1))]
    (is (equal? (make-color 1 1 1)
                (phong-lighting light mater position toEye normal))
         1e-4
        ))
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 10 -10) (make-color 1 1 1))]
    (is (equal? (make-color 0.7364 0.7364 0.7364)
                (phong-lighting light mater position toEye normal)
                1e-4)
        ))
  (let [toEye (make-vector 0 (- sqrt2_2) (- sqrt2_2))
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 10 -10) (make-color 1 1 1))]
    (is (equal? (make-color 1.6364 1.6364 1.6364)
                (phong-lighting light mater position toEye normal)
                1e-4)
        ))
  (let [toEye (make-vector 0 0 -1)
        normal (make-vector 0 0 -1)
        light (make-point-light (make-point 0 0 10) (make-color 1 1 1))]
    (is (equal? (make-color 0.1 0.1 0.1)
                (phong-lighting light mater position toEye normal)
                1e-4)
        ))
  )
