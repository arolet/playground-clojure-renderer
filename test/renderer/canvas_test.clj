(ns renderer.canvas_test
  (:require [clojure.test :refer :all]
            [clojure.string :as Strings]
            [renderer.canvas :refer :all]
            [renderer.texture.color :as Color]))

(deftest test-blank-canvas
  (is (= [[[0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
          [[0 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]] (:data (blank-canvas 5 2))))
  (is (= 5 (:width (blank-canvas 5 2))))
  (is (= 2 (:height (blank-canvas 5 2))))
  )

(def canvas-test (->Canvas [[[1.5 0 0] [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
                           [[0 0 0] [0 0 0] [0 0.5 0] [0 0 0] [0 0 0]]
                           [[0 0 0] [0 0 0] [0 0 0] [0 0 0] [-0.5 0 1]]] 5 3))

(deftest test-fn->canvas
  (is (equal (->Canvas [[[0 0 0] [0 0.1 0] [0 0.2 0]]
                        [[0.1 0 0] [0.1 0.1 0] [0.1 0.2 0]]] 3 2)
             (fn->canvas (fn [i j] (Color/make-color (/ i 10.0) (/ j 10.0) 0)) 3 2)))
  (is (equal (->Canvas [[[0 0 0] [0 0.1 0] [0 0.2 0]]
                        [[0.1 0 0] [0.1 0.1 0] [0.1 0.2 0]]] 3 2)
             (fn->canvas (fn [i j]
                           (if (= [i j] [0 0])
                             (Color/make-color 1.5 0 0)
                             (if (= [i j] [1 2])
                               (Color/make-color 0 0.5 0)
                               (if (= [i j] [2 4])
                                 (Color/make-color -0.5 0 1)
                                 (Color/make-color 0 0 0)
                                 )
                               ))
                           (Color/make-color (/ i 10.0) (/ j 10.0) 0)) 3 2)))
  (is (not (equal (->Canvas [[[0 0 0] [0 0.1 0.1] [0 0.2 0]]
                        [[0.1 0 0] [0.1 0.1 0] [0.1 0.2 0]]] 3 2)
                  (fn->canvas (fn [i j] (Color/make-color (/ i 10.0) (/ j 10.0) 0)) 3 2))))
  )

(deftest test-canvas->ppm
  (is (= ["P3" "5 3" "255"] (subvec (Strings/split-lines (canvas->ppm canvas-test)) 0 3)))
  (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
          "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
          "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"] (subvec (Strings/split-lines (canvas->ppm canvas-test)) 3)))
  )

(deftest test-row->ppm-line
  (is (= ["a v" "as d"] (row->ppm-line (Strings/split "a v as d" #" ") 4)))
  (is (= ["a v" "asd d"] (row->ppm-line (Strings/split "a v asd d" #" ") 5)))
  (is (= ["a v" "asd d"] (row->ppm-line (Strings/split "a v asd d" #" ") 6)))
  (is (= ["a v" "as d" "54 5" "2 56"] (row->ppm-line (Strings/split "a v as d 54 5 2 56" #" ") 4)))
  )

(deftest test-split-heap-error
  (is (> (count (row->ppm-line (repeat 9000 "100") 7)) 10) "Run on a big line to try to raise errors")
  )

(deftest test-canvas->ppm-splits-lines
  (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
          "153 255 204 153 255 204 153 255 204 153 255 204 153"
          "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
          "153 255 204 153 255 204 153 255 204 153 255 204 153"]
         (subvec (Strings/split-lines (canvas->ppm (color-canvas (Color/make-color 1 0.8 0.6) 10 4)))
                 3 7))))


(deftest test-canvas->ppm-ends-with-EOL
  (is (Strings/ends-with? (canvas->ppm canvas-test) "\n"))
  (is (Strings/ends-with? (canvas->ppm (color-canvas (Color/make-color 1 0.8 0.6) 10 4)) "\n"))
  )
