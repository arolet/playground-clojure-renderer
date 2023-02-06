(ns renderer.transformation_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer [->Mat inverse mDotVec matEqual]]
            [renderer.transformation :refer :all]
            [renderer.tuple :refer [equal makePoint makeVector]]))

(deftest testTranslation
  (let [p (makePoint -3 4 5)
        trans (translation 5 -3 2)]
    (is (equal (makePoint 2 1 7) (mDotVec trans p)) "translation moves points")
    (is (equal (makePoint -8 7 3) (mDotVec (inverse trans) p)) "translation moves points")
    )
  (let [v (makeVector -3 4 5)
        trans (translation 5 -3 2)]
    (is (equal v (mDotVec trans v)) "translation doesn't move vectors")
    (is (equal v (mDotVec (inverse trans) v)) "translation doesn't move vectors")
    )
  (is (matEqual (->Mat 4 4 [1 0 0 2
                            0 1 0 3
                            0 0 1 4
                            0 0 0 1])
                (translation 2 3 4)))
  )

(deftest testScaling
  (let [mat (scaling 2 3 4)]
    (is (equal (makePoint -8 18 32) (mDotVec mat (makePoint -4 6 8))))
    (is (equal (makeVector -8 18 32) (mDotVec mat (makeVector -4 6 8))))
    )
  (is (equal (makePoint -2 3 4) (mDotVec (scaling -1 1 1) (makePoint 2 3 4))))
  )

(def sqrt2_2 (/ (Math/sqrt 2) 2))

(deftest testRotationX
  (let [p (makePoint 0 1 0)]
    (is (equal (makePoint 0 sqrt2_2 sqrt2_2) (mDotVec (rotationX (/ Math/PI 4)) p)))
    (is (equal (makePoint 0 0 1) (mDotVec (rotationX (/ Math/PI 2)) p)))
    )
  )

(deftest testRotationY
  (let [p (makePoint 0 0 1)]
    (is (equal (makePoint sqrt2_2 0 sqrt2_2) (mDotVec (rotationY (/ Math/PI 4)) p)))
    (is (equal (makePoint 1 0 0) (mDotVec (rotationY (/ Math/PI 2)) p)))
    )
  )

(deftest testRotationZ
  (let [p (makePoint 0 1 0)]
    (is (equal (makePoint (- sqrt2_2) sqrt2_2 0) (mDotVec (rotationZ (/ Math/PI 4)) p)))
    (is (equal (makePoint (- 1) 0 0) (mDotVec (rotationZ (/ Math/PI 2)) p)))
    )
  )

(deftest testSheering
  (let [p (makePoint 2 3 4)]
    (is (equal (makePoint 5 3 4) (mDotVec (sheering 1 0 0 0 0 0) p)))
    (is (equal (makePoint 6 3 4) (mDotVec (sheering 0 1 0 0 0 0) p)))
    (is (equal (makePoint 2 5 4) (mDotVec (sheering 0 0 1 0 0 0) p)))
    (is (equal (makePoint 2 7 4) (mDotVec (sheering 0 0 0 1 0 0) p)))
    (is (equal (makePoint 2 3 6) (mDotVec (sheering 0 0 0 0 1 0) p)))
    (is (equal (makePoint 2 3 7) (mDotVec (sheering 0 0 0 0 0 1) p)))
    )
  )

(deftest testChainTransformations
  (let [p (makePoint 1 0 1)
        rot (rotationX (/ Math/PI 2))
        scal (scaling 5 5 5)
        trans (translation 10 5 7)
        p2 (mDotVec rot p)
        p3 (mDotVec scal p2)
        p4 (mDotVec trans p3)
        expect (makePoint 15 0 7)]
    (is (equal (makePoint 1 -1 0) p2))
    (is (equal (makePoint 5 -5 0) p3))
    (is (equal expect p4))
    (is (equal p2 (mDotVec (chain rot) p)))
    (is (equal p3 (mDotVec (chain rot scal) p)))
    (is (equal expect (mDotVec (chain rot scal trans) p)))
    )
  )
