(ns tests.renderer.tuple
    (:require [clojure.test :as test]
      [renderer.tuple :as tuple]))

(defn makeTestV []
      (tuple/makeVector 4.3 -4.2 3.1))
(defn makeTestP []
      (tuple/makePoint 4.3 -4.2 3.1))


(test/deftest testTupleConstructor
              (test/is (= (:x (makeTestV) 4.3)))
              (test/is (= (:y (makeTestV) -4.2)))
              (test/is (= (:z (makeTestV) 3.1)))
              (test/is (= (:w (makeTestV) 0.0)))
              (test/is (= (:w (makeTestP) 1.0)))
              )

(test/deftest testIsVector
              (test/is (tuple/isVector (makeTestV)))
              (test/is (not (tuple/isVector (makeTestP))))
              )

(test/deftest testIsPoint
              (test/is (tuple/isPoint (makeTestP)))
              (test/is (not (tuple/isPoint (makeTestV))))
              )

(test/deftest testTupleEqual
              (test/is (= (makeTestP) (makeTestP)))
              (test/is (= (makeTestV) (makeTestV)))
              (test/is (not (= (makeTestP) (makeTestV))))
              (test/is (not (= (makeTestV) (makeTestP))))
              (test/is (not (= (tuple/makePoint 4.0 -4.2 3.1) (makeTestP))))
              (test/is (not (= (tuple/makePoint 4.3 -0.2 3.1) (makeTestP))))
              (test/is (not (= (tuple/makePoint 4.3 -4.2 0.1) (makeTestP))))
              )
