(ns renderer.matrix_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer :all]
            [renderer.tuple :as Tuple]))

(def testData1 [1 2 3 4 5.5 6.5 7.5 8.5 9 10 11 12 13.5 14.5 15.5 16.5])
(def testMat1 (->Mat 4 4 testData1))
(def mat22 (->Mat 2 2 [-3 5 1 -2]))
(def mat33 (->Mat 3 3 [-3 5 0 1 -2 -7 0 1 1]))


(deftest testMakeMatrix
  (is (matEqual testMat1 testMat1))
  (is (== 1 (mGet testMat1 0 0)))
  (is (== 2 (mGet testMat1 0 1)))
  (is (== 3 (mGet testMat1 0 2)))
  (is (== 4 (mGet testMat1 0 3)))
  (is (== 5.5 (mGet testMat1 1 0)))
  (is (== 6.5 (mGet testMat1 1 1)))
  (is (== 14.5 (mGet testMat1 3 1)))
  (is (== 15.5 (mGet testMat1 3 2)))
  (is (== 16.5 (mGet testMat1 3 3)))
  (is (== -3 (mGet mat22 0 0)))
  (is (== 5 (mGet mat22 0 1)))
  (is (== 1 (mGet mat22 1 0)))
  (is (== -2 (mGet mat22 1 1)))
  (is (== -3 (mGet mat33 0 0)))
  (is (== -2 (mGet mat33 1 1)))
  (is (== 1 (mGet mat33 2 2)))
  (is (not (matEqual testMat1 (->Mat 3 4 testData1))))
  (is (not (matEqual testMat1 (->Mat 4 3 testData1))))
  (is (not (matEqual testMat1 (->Mat 4 4 (assoc testData1 3 -1)))))
  )

(deftest testGetRow
  (is (Tuple/equal [1 2 3 4] (getRow testMat1 0)))
  (is (Tuple/equal [13.5 14.5 15.5 16.5] (getRow testMat1 3)))
  (is (Tuple/equal [1 -2] (getRow mat22 1)))
  (is (Tuple/equal [1 -2 3] (getRow (->Mat 3 2 [0 0 0 1 -2 3]) 1)))
  )

(deftest testGetCol
  (is (Tuple/equal [1 5.5 9 13.5] (getCol testMat1 0)))
  (is (Tuple/equal [4 8.5 12 16.5] (getCol testMat1 3)))
  (is (Tuple/equal [5 -2] (getCol mat22 1)))
  (is (Tuple/equal [0 -2] (getCol (->Mat 3 2 [0 0 0 1 -2 3]) 1)))
  )

(deftest testMDotVec
  (is (Tuple/equal [10 28 42 60] (mDotVec testMat1 [1 1 1 1])))
  (is (Tuple/equal [0 0 0 0] (mDotVec testMat1 [1 -1 -1 1])))
  (is (Tuple/equal [2 2 2 2] (mDotVec testMat1 [-1 1 -1 1])))
  )

(deftest testMDotVecL
  (is (Tuple/equal [29 33 37 41] (mDotVecL testMat1 [1 1 1 1])))
  (is (Tuple/equal [0 0 0 0] (mDotVecL testMat1 [1 -1 -1 1])))
  (is (Tuple/equal [9 9 9 9] (mDotVecL testMat1 [-1 1 -1 1])))
  )

(deftest testMatDot
  (is (matEqual (->Mat 4 4 [20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42])
                (mDot (->Mat 4 4 [1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2])
                      (->Mat 4 4 [-2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8])))
      )

  (is (matEqual (->Mat 2 2 [11 13 20 31])
                (mDot (->Mat 3 2 [1 2 3 4 5 6])
                      (->Mat 2 3 [-2 1 2 3 3 2])))
      )
  )
