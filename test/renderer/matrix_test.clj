(ns renderer.matrix_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer :all]
            [renderer.tuple :as Tuple]))

(def testData1 [1 2 3 4 5.5 6.5 7.5 8.5 9 10 11 12 13.5 14.5 15.5 16.5])
(def testMat1 (->Mat 4 4 testData1))
(def mat22 (->Mat 2 2 [-3 5 1 -2]))
(def mat33 (->Mat 3 3 [-3 5 0 1 -2 -7 0 1 1]))


(deftest testMakeIdentity
  (is (matEqual (->Mat 2 2 [1 0 0 1]) (makeIdentity 2)))
  (is (matEqual (->Mat 3 3 [1 0 0 0 1 0 0 0 1]) (makeIdentity 3)))
  (is (matEqual (->Mat 4 4 [1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]) (makeIdentity 4)))
  )

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
  (is (not (matEqual testMat1 (->Mat 4 3 testData1))))
  (is (not (matEqual testMat1 (->Mat 3 4 testData1))))
  (is (not (matEqual testMat1 (->Mat 4 4 (assoc testData1 3 -1)))))
  )

(deftest testGetRow
  (is (Tuple/equal [1 2 3 4] (getRow testMat1 0)))
  (is (Tuple/equal [13.5 14.5 15.5 16.5] (getRow testMat1 3)))
  (is (Tuple/equal [1 -2] (getRow mat22 1)))
  (is (Tuple/equal [1 -2 3] (getRow (->Mat 2 3 [0 0 0 1 -2 3]) 1)))
  )

(deftest testGetCol
  (is (Tuple/equal [1 5.5 9 13.5] (getCol testMat1 0)))
  (is (Tuple/equal [4 8.5 12 16.5] (getCol testMat1 3)))
  (is (Tuple/equal [5 -2] (getCol mat22 1)))
  (is (Tuple/equal [0 -2] (getCol (->Mat 2 3 [0 0 0 1 -2 3]) 1)))
  )

(deftest testMDotVec
  (is (Tuple/equal [10 28 42 60] (mDotVec testMat1 [1 1 1 1])))
  (is (Tuple/equal [0 0 0 0] (mDotVec testMat1 [1 -1 -1 1])))
  (is (Tuple/equal [2 2 2 2] (mDotVec testMat1 [-1 1 -1 1])))
  (is (Tuple/equal [-2 -5] (mDotVec (->Mat 2 3 [1 2 3 4 5 6]) [-1 1 -1])))
  )

(deftest testMDotVecL
  (is (Tuple/equal [29 33 37 41] (mDotVecL testMat1 [1 1 1 1])))
  (is (Tuple/equal [0 0 0 0] (mDotVecL testMat1 [1 -1 -1 1])))
  (is (Tuple/equal [9 9 9 9] (mDotVecL testMat1 [-1 1 -1 1])))
  (is (Tuple/equal [3 3 3] (mDotVecL (->Mat 2 3 [1 2 3 4 5 6]) [-1 1])))
  )

(deftest testMatDotWithCopies
  (is (matEqual (->Mat 4 4 [20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42])
                (mDotWithCopies (->Mat 4 4 [1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2])
                                (->Mat 4 4 [-2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8])))
      )
  (is (matEqual (->Mat 2 2 [11 13 20 31])
                (mDotWithCopies (->Mat 2 3 [1 2 3 4 5 6])
                                (->Mat 3 2 [-2 1 2 3 3 2])))
      )
  )

(deftest testMatDot
  (is (matEqual (->Mat 4 4 [20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42])
                (mDot (->Mat 4 4 [1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2])
                      (->Mat 4 4 [-2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8])))
      )
  (is (matEqual (->Mat 2 2 [11 13 20 31])
                (mDot (->Mat 2 3 [1 2 3 4 5 6])
                      (->Mat 3 2 [-2 1 2 3 3 2])))
      )
  )

(deftest testIdentityDot
  (is (let [expected (->Mat 3 3 [1 2 3 4 5 6 7 8 9])
            actual (mDot (makeIdentity 3) expected)]
        (matEqual expected actual)))
  (is (let [expected (->Mat 3 3 [1 2 3 4 5 6 7 8 9])
            actual (mDot (makeIdentity 3) expected)]
        (matEqual expected actual)))
  (is (let [expected (->Mat 2 2 [1 2 3 4])
            actual (mDot (makeIdentity 2) expected)]
        (matEqual expected actual)))
  (is (let [expected (->Mat 4 4 [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
            actual (mDot (makeIdentity 4) expected)]
        (matEqual expected actual)))
  (is (Tuple/equal [3 -2 5] (mDotVecL (makeIdentity 3) [3 -2 5])))
  (is (Tuple/equal [3 -2 5] (mDotVec (makeIdentity 3) [3 -2 5])))
  )

(deftest testTranspose
  (is (matEqual (->Mat 2 2 [1 3 2 4]) (transpose (->Mat 2 2 [1 2 3 4]))))
  (is (matEqual (->Mat 2 2 [1 3 2 4]) (transpose (->Mat 2 2 [1 2 3 4]))))
  (is (matEqual (->Mat 2 3 [1 3 5 2 4 6]) (transpose (->Mat 3 2 [1 2 3 4 5 6]))))
  (is (matEqual (makeIdentity 3) (transpose (makeIdentity 3))))
  (is (matEqual (->Mat 4 4 [0 9 1 0 9 8 8 0 3 0 5 5 0 8 3 8])
                (transpose (->Mat 4 4 [0 9 3 0 9 8 0 8 1 8 5 3 0 0 5 8]))))
  )

(deftest testTransposeSelfInverse
  (is (let [expected (->Mat 3 3 [1 2 3 4 5 6 7 8 9])
            actual (transpose (transpose expected))]
        (matEqual expected actual)))
  (is (let [expected (->Mat 3 2 [1 3 5 2 4 6])
            actual (transpose (transpose expected))]
        (matEqual expected actual)))
  )

(deftest test2x2Determinant
  (is (== 1 (det (makeIdentity 2))))
  (is (== 0 (det (->Mat 2 2 [1 1 1 1]))))
  (is (== 17 (det (->Mat 2 2 [1 5 -3 2]))))
  )

(deftest testDelRowCol
  (is (matEqual (->Mat 2 2 [-3 2 0 6]) (delRowCol (->Mat 3 3 [1 5 0 -3 2 7 0 6 -3]) 0 2)))
  (is (matEqual (->Mat 3 3 [-6 1 6 -8 8 6 -7 -1 1])
                (delRowCol (->Mat 4 4 [-6 1 1 6 -8 5 8 6 -1 0 8 2 -7 1 -1 1]) 2 1)))
  )

(deftest testMinor
  (let [mat (->Mat 3 3 [3 5 0 2 -1 -7 6 -1 5])
        deleted (delRowCol mat 1 0)]
    (is (== 25 (det deleted)))
    (is (== 25 (minor mat 1 0)))
    )
  )

(deftest testCofactor
  (let [mat (->Mat 3 3 [3 5 0 2 -1 -7 6 -1 5])]
    (is (== -12 (minor mat 0 0)))
    (is (== -12 (cofactor mat 0 0)))
    (is (== 25 (minor mat 1 0)))
    (is (== -25 (cofactor mat 1 0)))
    )
  )

(deftest testDetDimOver2
  (let [mat (->Mat 3 3 [1 2 6 -5 8 -4 2 6 4])]
    (is (== 56) (cofactor mat 0 0))
    (is (== 12) (cofactor mat 0 1))
    (is (== -46) (cofactor mat 0 2))
    (is (== -196) (det mat))
    )
  (let [mat (->Mat 4 4 [-2 -8 3 5 -3 1 7 3 1 2 -9 6 -6 7 7 -9])]
    (is (== 690) (cofactor mat 0 0))
    (is (== 447) (cofactor mat 0 1))
    (is (== 210) (cofactor mat 0 2))
    (is (== 51) (cofactor mat 0 3))
    (is (== -4071) (det mat))
    )
  (is (== -2120 (det (->Mat 4 4 [6 4 4 4 5 5 7 6 4 -9 3 -7 9 1 7 -6]))))
  (is (== 0 (det (->Mat 4 4 [1 2 3 4 5 6 7 8 9 10 11 12 0 0 0 0]))))
  )
