(ns renderer.canvas_test
  (:require [clojure.test :refer :all]
            [clojure.string :as Strings]
            [renderer.canvas :refer :all]
            [renderer.color :as Color]))

(deftest testBlankCanvas
  (is (= [[[0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
          [[0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]] (:data (blankCanvas 5 2))))
  (is (= 5 (:width (blankCanvas 5 2))))
  (is (= 2 (:height (blankCanvas 5 2))))
  )

(def canvasTest (->Canvas [[[1.5 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
                           [[0 0 0] [0 0 0] [0 0.5 0] [0 0 0] [0 0 0]]
                           [[0 0 0] [0 0 0] [0 0 0] [0 0 0] [-0.5 0 1]]] 5 3))

(deftest testFnToCanvas
  (is (equal (->Canvas [[[0 0 0] [0 0.1 0] [0 0.2 0]]
                        [[0.1 0 0] [0.1 0.1 0] [0.1 0.2 0]]] 3 2)
             (fnToCanvas (fn [i j] (Color/makeColor (/ i 10.0) (/ j 10.0) 0)) 3 2)))
  (is (equal (->Canvas [[[0 0 0] [0 0.1 0] [0 0.2 0]]
                        [[0.1 0 0] [0.1 0.1 0] [0.1 0.2 0]]] 3 2)
             (fnToCanvas (fn [i j]
                           (if (= [i j] [0 0])
                             (Color/makeColor 1.5 0 0)
                             (if (= [i j] [1 2])
                               (Color/makeColor 0 0.5 0)
                               (if (= [i j] [2 4])
                                 (Color/makeColor -0.5 0 1)
                                 (Color/makeColor 0 0 0)
                                 )
                               ))
                           (Color/makeColor (/ i 10.0) (/ j 10.0) 0)) 3 2)))
  (is (not (equal (->Canvas [[[0 0 0] [0 0.1 0.1] [0 0.2 0]]
                        [[0.1 0 0] [0.1 0.1 0] [0.1 0.2 0]]] 3 2)
             (fnToCanvas (fn [i j] (Color/makeColor (/ i 10.0) (/ j 10.0) 0)) 3 2))))
  )

(deftest testCanvasToPpm
  (is (= ["P3" "5 3" "255"] (subvec (Strings/split-lines (toPpmString canvasTest)) 0 3)))
  (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
          "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
          "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"] (subvec (Strings/split-lines (toPpmString canvasTest)) 3)))
  )

(deftest testSplitLine
  (is (= ["a v" "as d"] (splitLine "a v as d" 4)))
  (is (= ["a v" "asd d"] (splitLine "a v asd d" 5)))
  (is (= ["a v" "asd d"] (splitLine "a v asd d" 6)))
  )

(deftest testCanvasToPpmSplitsLines
  (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
          "153 255 204 153 255 204 153 255 204 153 255 204 153"
          "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
          "153 255 204 153 255 204 153 255 204 153 255 204 153"]
         (subvec (Strings/split-lines (toPpmString (colorCanvas (Color/makeColor 1 0.8 0.6) 10 4)))
                 3 7))))


(deftest testCanvasToPpmEndsWithEOL
  (is (Strings/ends-with? (toPpmString canvasTest) "\n"))
  (is (Strings/ends-with? (toPpmString (colorCanvas (Color/makeColor 1 0.8 0.6) 10 4)) "\n"))
  )
