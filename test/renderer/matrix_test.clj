(ns renderer.matrix_test
  (:require [clojure.test :refer :all]
            [renderer.matrix :refer :all]
            [renderer.tuple :as Tuple]))

(def testData1 [1 2 3 4 5.5 6.5 7.5 8.5 9 10 11 12 13.5 14.5 15.5 16.5])
(def testMat1 (->Mat 4 4 testData1))
(def mat22 (->Mat 2 2 [-3 5 1 -2]))
(def mat33 (->Mat 3 3 [-3 5 0 1 -2 -7 0 1 1]))


(deftest test-make-identity
  (is (mat-equal? (->Mat 2 2 [1 0 0 1]) (make-identity 2)))
  (is (mat-equal? (->Mat 3 3 [1 0 0 0 1 0 0 0 1]) (make-identity 3)))
  (is (mat-equal? (->Mat 4 4 [1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1]) (make-identity 4)))
  )

(deftest test-make-matrix
  (is (mat-equal? testMat1 testMat1))
  (is (== 1 (m-get testMat1 0 0)))
  (is (== 2 (m-get testMat1 0 1)))
  (is (== 3 (m-get testMat1 0 2)))
  (is (== 4 (m-get testMat1 0 3)))
  (is (== 5.5 (m-get testMat1 1 0)))
  (is (== 6.5 (m-get testMat1 1 1)))
  (is (== 14.5 (m-get testMat1 3 1)))
  (is (== 15.5 (m-get testMat1 3 2)))
  (is (== 16.5 (m-get testMat1 3 3)))
  (is (== -3 (m-get mat22 0 0)))
  (is (== 5 (m-get mat22 0 1)))
  (is (== 1 (m-get mat22 1 0)))
  (is (== -2 (m-get mat22 1 1)))
  (is (== -3 (m-get mat33 0 0)))
  (is (== -2 (m-get mat33 1 1)))
  (is (== 1 (m-get mat33 2 2)))
  (is (not (mat-equal? testMat1 (->Mat 4 3 testData1))))
  (is (not (mat-equal? testMat1 (->Mat 3 4 testData1))))
  (is (not (mat-equal? testMat1 (->Mat 4 4 (assoc testData1 3 -1)))))
  )

(deftest test-get-row
  (is (Tuple/equal? [1 2 3 4] (get-row testMat1 0)))
  (is (Tuple/equal? [13.5 14.5 15.5 16.5] (get-row testMat1 3)))
  (is (Tuple/equal? [1 -2] (get-row mat22 1)))
  (is (Tuple/equal? [1 -2 3] (get-row (->Mat 2 3 [0 0 0 1 -2 3]) 1)))
  )

(deftest test-get-col
  (is (Tuple/equal? [1 5.5 9 13.5] (get-col testMat1 0)))
  (is (Tuple/equal? [4 8.5 12 16.5] (get-col testMat1 3)))
  (is (Tuple/equal? [5 -2] (get-col mat22 1)))
  (is (Tuple/equal? [0 -2] (get-col (->Mat 2 3 [0 0 0 1 -2 3]) 1)))
  )

(deftest test-m-dot-vec
  (is (Tuple/equal? [10 28 42 60] (m-dot-vec testMat1 [1 1 1 1])))
  (is (Tuple/equal? [0 0 0 0] (m-dot-vec testMat1 [1 -1 -1 1])))
  (is (Tuple/equal? [2 2 2 2] (m-dot-vec testMat1 [-1 1 -1 1])))
  (is (Tuple/equal? [-2 -5] (m-dot-vec (->Mat 2 3 [1 2 3 4 5 6]) [-1 1 -1])))
  )

(deftest test-m-dot-vec-l
  (is (Tuple/equal? [29 33 37 41] (m-dot-vec-l testMat1 [1 1 1 1])))
  (is (Tuple/equal? [0 0 0 0] (m-dot-vec-l testMat1 [1 -1 -1 1])))
  (is (Tuple/equal? [9 9 9 9] (m-dot-vec-l testMat1 [-1 1 -1 1])))
  (is (Tuple/equal? [3 3 3] (m-dot-vec-l (->Mat 2 3 [1 2 3 4 5 6]) [-1 1])))
  )

(deftest test-mat-dot-with-copies
  (is (mat-equal? (->Mat 4 4 [20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42])
                  (m-dot-with-copies (->Mat 4 4 [1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2])
                                   (->Mat 4 4 [-2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8])))
      )
  (is (mat-equal? (->Mat 2 2 [11 13 20 31])
                  (m-dot-with-copies (->Mat 2 3 [1 2 3 4 5 6])
                                   (->Mat 3 2 [-2 1 2 3 3 2])))
      )
  )

(deftest test-mat-dot
  (is (mat-equal? (->Mat 4 4 [20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42])
                  (m-dot (->Mat 4 4 [1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2])
                       (->Mat 4 4 [-2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8])))
      )
  (is (mat-equal? (->Mat 2 2 [11 13 20 31])
                  (m-dot (->Mat 2 3 [1 2 3 4 5 6])
                       (->Mat 3 2 [-2 1 2 3 3 2])))
      )
  )

(deftest test-identity-dot
  (is (let [expected (->Mat 3 3 [1 2 3 4 5 6 7 8 9])
            actual (m-dot (make-identity 3) expected)]
        (mat-equal? expected actual)))
  (is (let [expected (->Mat 3 3 [1 2 3 4 5 6 7 8 9])
            actual (m-dot (make-identity 3) expected)]
        (mat-equal? expected actual)))
  (is (let [expected (->Mat 2 2 [1 2 3 4])
            actual (m-dot (make-identity 2) expected)]
        (mat-equal? expected actual)))
  (is (let [expected (->Mat 4 4 [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
            actual (m-dot (make-identity 4) expected)]
        (mat-equal? expected actual)))
  (is (Tuple/equal? [3 -2 5] (m-dot-vec-l (make-identity 3) [3 -2 5])))
  (is (Tuple/equal? [3 -2 5] (m-dot-vec (make-identity 3) [3 -2 5])))
  )

(deftest test-transpose
  (is (mat-equal? (->Mat 2 2 [1 3 2 4]) (transpose (->Mat 2 2 [1 2 3 4]))))
  (is (mat-equal? (->Mat 2 2 [1 3 2 4]) (transpose (->Mat 2 2 [1 2 3 4]))))
  (is (mat-equal? (->Mat 2 3 [1 3 5 2 4 6]) (transpose (->Mat 3 2 [1 2 3 4 5 6]))))
  (is (mat-equal? (make-identity 3) (transpose (make-identity 3))))
  (is (mat-equal? (->Mat 4 4 [0 9 1 0 9 8 8 0 3 0 5 5 0 8 3 8])
                  (transpose (->Mat 4 4 [0 9 3 0 9 8 0 8 1 8 5 3 0 0 5 8]))))
  )

(deftest test-transpose-self-inverse
  (is (let [expected (->Mat 3 3 [1 2 3 4 5 6 7 8 9])
            actual (transpose (transpose expected))]
        (mat-equal? expected actual)))
  (is (let [expected (->Mat 3 2 [1 3 5 2 4 6])
            actual (transpose (transpose expected))]
        (mat-equal? expected actual)))
  )

(deftest test-2x2-determinant
  (is (== 1 (det (make-identity 2))))
  (is (== 0 (det (->Mat 2 2 [1 1 1 1]))))
  (is (== 17 (det (->Mat 2 2 [1 5 -3 2]))))
  )

(deftest test-del-row-col
  (is (mat-equal? (->Mat 2 2 [-3 2 0 6]) (del-row-col (->Mat 3 3 [1 5 0 -3 2 7 0 6 -3]) 0 2)))
  (is (mat-equal? (->Mat 3 3 [-6 1 6 -8 8 6 -7 -1 1])
                  (del-row-col (->Mat 4 4 [-6 1 1 6 -8 5 8 6 -1 0 8 2 -7 1 -1 1]) 2 1)))
  )

(deftest test-minor
  (let [mat (->Mat 3 3 [3 5 0 2 -1 -7 6 -1 5])
        deleted (del-row-col mat 1 0)]
    (is (== 25 (det deleted)))
    (is (== 25 (minor mat 1 0)))
    )
  )

(deftest test-cofactor
  (let [mat (->Mat 3 3 [3 5 0 2 -1 -7 6 -1 5])]
    (is (== -12 (minor mat 0 0)))
    (is (== -12 (cofactor mat 0 0)))
    (is (== 25 (minor mat 1 0)))
    (is (== -25 (cofactor mat 1 0)))
    )
  )

(deftest test-det-dim-over-2
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

(deftest test-invert
  (let [mat (->Mat 4 4 [-5 2 6 -8 1 -5 1 8 7 7 -6 -7 1 -3 7 4])
        inverted (invert mat)]
    (is (== 532 (det mat)))
    (is (== -160 (cofactor mat 2 3)))
    (is (== (/ -160.0 532) (m-get inverted 3 2)))
    (is (== 105 (cofactor mat 3 2)))
    (is (== (/ 105.0 532) (m-get inverted 2 3)))
    (is (mat-equal? (->Mat 4 4 [0.21805 0.45113 0.2406 -0.04511
                              -0.80827 -1.45677 -0.44361 0.52068
                              -0.07895 -0.22368 -0.05263 0.19737
                              -0.52256 -0.81391 -0.30075 0.30639])
                    inverted
                    1e-5))
    )
  (is (mat-equal? (make-identity 4) (invert (make-identity 4))))
  (is (mat-equal? (->Mat 4 4 [-0.15385 -0.15385 -0.28205 -0.53846
                            -0.07692 0.12308 0.02564 0.03077
                            0.35897 0.35897 0.43590 0.92308
                            -0.69231 -0.69231 -0.76923 -1.92308])
                  (invert (->Mat 4 4 [8 -5 9 2 7 5 6 1 -6 0 9 6 -3 0 -9 -4]))
                  1e-5))
  (is (mat-equal? (->Mat 4 4 [-0.04074 -0.07778 0.14444 -0.22222
                            -0.07778 0.03333 0.36667 -0.33333
                            -0.02901 -0.14630 -0.10926 0.12963
                            0.17778 0.06667 -0.26667 0.33333])
                  (invert (->Mat 4 4 [9 3 0 9
                                     -5 -2 -6 -3
                                     -4 9 6 4
                                     -7 6 6 2]))
                  1e-5))
  (let [a (->Mat 4 4 [3 -9 7 3 3 -8 2 -9 -4 4 4 1 -6 5 -1 1])
        b (->Mat 4 4 [8 2 2 2 3 -1 7 0 7 0 5 4 6 -2 0 5])
        c (m-dot a b)]
    (is (mat-equal? a (m-dot c (invert b))))
    (is (mat-equal? a (invert (invert a))))
    (is (mat-equal? b (invert (invert b))))
    (is (mat-equal? (transpose (invert b)) (invert (transpose b))))
    )
  (is (thrown? AssertionError (invert (->Mat 3 2 [1 2 3 4 5 6]))))
  (is (thrown? AssertionError (invert (->Mat 4 4 [1 2 3 4 5 6 7 8 9 10 11 12 0 0 0 0]))))
  )
