(ns renderer.screen_test
  (:require [clojure.test :refer :all]
            [renderer.screen :refer :all]
            [renderer.tuple :as Tuple]
            [renderer.utils :refer :all]
            [renderer.tuple :refer [equal? make-point make-vector]]
            [renderer.matrix :refer [mat-equal? make-identity ->Mat]]
            [renderer.transformation :refer [scaling translation]]))

(deftest test-make-screen
  (let [screen (make-screen 100 100 (make-point 1 1 -1) (make-point 1 1 1)
                            (make-point 1 -1 -1))]
    (is (equal? (make-point 1 1 -1) (:origin screen)))
    (is (equal? (make-vector 0 0 0.02) (:vX screen)))
    (is (equal? (make-vector 0 -0.02 0) (:vY screen)))
    )
  (let [screen (make-screen 50 100 (make-point 1 1 -1) (make-point 1 1 1)
                            (make-point 1 -1 -1))]
    (is (equal? (make-point 1 1 -1) (:origin screen)))
    (is (equal? (make-vector 0 0 0.04) (:vX screen)))
    (is (equal? (make-vector 0 -0.02 0) (:vY screen)))
    )
  (let [screen (make-screen 100 25 (make-point 1 1 -1) (make-point 1 1 1)
                            (make-point 1 -1 -1))]
    (is (equal? (make-point 1 1 -1) (:origin screen)))
    (is (equal? (make-vector 0 0 0.02) (:vX screen)))
    (is (equal? (make-vector 0 -0.08 0) (:vY screen)))
    )
  )

(deftest test-get-pixel-point
  (let [screen (make-screen 100 100 (make-point 1 1 -1) (make-point 1 1 1)
                            (make-point 1 -1 -1))]
    (is (equal? (make-point 1 0.99 -0.99) (get-pixel-point screen 0 0)))
    (is (equal? (make-point 1 -0.01 0.01) (get-pixel-point screen 50 50)))
    (is (equal? (make-point 1 -0.99 0.99) (get-pixel-point screen 99 99)))
    )
  )


(deftest test-get-pixel-ray
  (let [screen (make-screen 100 100 (make-point 1 1 -1) (make-point 1 1 1)
                            (make-point 1 -1 -1))
        camera (->Camera (make-point 0 0 0) screen)]
    (is (equal? (Tuple/normalize (make-vector 1 0.99 -0.99)) (:direction (get-pixel-ray camera 0 0))))
    (is (equal? (Tuple/normalize (make-vector 1 -0.01 0.01)) (:direction (get-pixel-ray camera 50 50))))
    (is (equal? (Tuple/normalize (make-vector 1 -0.99 0.99)) (:direction (get-pixel-ray camera 99 99))))
    )
  )

(deftest test-view-transform
  (is (mat-equal? (make-identity 4) (view-transform (make-point 0 0 0)
                                                    (make-point 0 0 -1)
                                                    (make-vector 0 1 0))
                  1e-5))
  (is (mat-equal? (scaling -1 1 -1) (view-transform (make-point 0 0 0)
                                                    (make-point 0 0 1)
                                                    (make-vector 0 1 0))
                  1e-5))
  (is (mat-equal? (translation 0 0 -8) (view-transform (make-point 0 0 8)
                                                       (make-point 0 0 0)
                                                       (make-vector 0 1 0))
                  1e-5))
  (is (mat-equal? (->Mat 4
                         4
                         [-0.50709 0.50709 0.67612 -2.36643
                          0.76772 0.60609 0.12122 -2.82843
                          -0.35857 0.59761 -0.71714 0
                          0 0 0 1])
                  (view-transform (make-point 1 3 2)
                                  (make-point 4 -2 8)
                                  (make-vector 1 1 0))
                  1e-5
                  ))
  )
