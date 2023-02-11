(ns renderer.transformation_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer [->Mat invert m-dot-vec mat-equal?]]
            [renderer.transformation :refer :all]
            [renderer.tuple :refer [equal? make-point make-vector]]))

(deftest test-translation
  (let [p (make-point -3 4 5)
        trans (translation 5 -3 2)]
    (is (equal? (make-point 2 1 7) (m-dot-vec trans p)) "translation moves points")
    (is (equal? (make-point -8 7 3) (m-dot-vec (invert trans) p)) "translation moves points")
    )
  (let [v (make-vector -3 4 5)
        trans (translation 5 -3 2)]
    (is (equal? v (m-dot-vec trans v)) "translation doesn't move vectors")
    (is (equal? v (m-dot-vec (invert trans) v)) "translation doesn't move vectors")
    )
  (is (mat-equal? (->Mat 4 4 [1 0 0 2
                            0 1 0 3
                            0 0 1 4
                            0 0 0 1])
                  (translation 2 3 4)))
  )

(deftest test-scaling
  (let [mat (scaling 2 3 4)]
    (is (equal? (make-point -8 18 32) (m-dot-vec mat (make-point -4 6 8))))
    (is (equal? (make-vector -8 18 32) (m-dot-vec mat (make-vector -4 6 8))))
    )
  (is (equal? (make-point -2 3 4) (m-dot-vec (scaling -1 1 1) (make-point 2 3 4))))
  )

(def sqrt2_2 (/ (Math/sqrt 2) 2))

(deftest test-rotation-x
  (let [p (make-point 0 1 0)]
    (is (equal? (make-point 0 sqrt2_2 sqrt2_2) (m-dot-vec (rotation-x (/ Math/PI 4)) p)))
    (is (equal? (make-point 0 0 1) (m-dot-vec (rotation-x (/ Math/PI 2)) p)))
    )
  )

(deftest test-rotation-y
  (let [p (make-point 0 0 1)]
    (is (equal? (make-point sqrt2_2 0 sqrt2_2) (m-dot-vec (rotation-y (/ Math/PI 4)) p)))
    (is (equal? (make-point 1 0 0) (m-dot-vec (rotation-y (/ Math/PI 2)) p)))
    )
  )

(deftest test-rotation-z
  (let [p (make-point 0 1 0)]
    (is (equal? (make-point (- sqrt2_2) sqrt2_2 0) (m-dot-vec (rotation-z (/ Math/PI 4)) p)))
    (is (equal? (make-point (- 1) 0 0) (m-dot-vec (rotation-z (/ Math/PI 2)) p)))
    )
  )

(deftest test-sheering
  (let [p (make-point 2 3 4)]
    (is (equal? (make-point 5 3 4) (m-dot-vec (sheering 1 0 0 0 0 0) p)))
    (is (equal? (make-point 6 3 4) (m-dot-vec (sheering 0 1 0 0 0 0) p)))
    (is (equal? (make-point 2 5 4) (m-dot-vec (sheering 0 0 1 0 0 0) p)))
    (is (equal? (make-point 2 7 4) (m-dot-vec (sheering 0 0 0 1 0 0) p)))
    (is (equal? (make-point 2 3 6) (m-dot-vec (sheering 0 0 0 0 1 0) p)))
    (is (equal? (make-point 2 3 7) (m-dot-vec (sheering 0 0 0 0 0 1) p)))
    )
  )

(deftest test-chain-transformations
  (let [p (make-point 1 0 1)
        rot (rotation-x (/ Math/PI 2))
        scal (scaling 5 5 5)
        trans (translation 10 5 7)
        p2 (m-dot-vec rot p)
        p3 (m-dot-vec scal p2)
        p4 (m-dot-vec trans p3)
        expect (make-point 15 0 7)]
    (is (equal? (make-point 1 -1 0) p2))
    (is (equal? (make-point 5 -5 0) p3))
    (is (equal? expect p4))
    (is (equal? p2 (m-dot-vec (chain rot) p)))
    (is (equal? p3 (m-dot-vec (chain rot scal) p)))
    (is (equal? expect (m-dot-vec (chain rot scal trans) p)))
    )
  )
