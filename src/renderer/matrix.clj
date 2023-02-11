(ns renderer.matrix
  (:require [renderer.tuple :refer [equal? dot EPSILON]]))

(defrecord Mat [height width data])

(defn equal-1-else-0 [a b]
  (if (== a b) 1 0))

(defn make-mat-with-fun [height width fun]
  (let [data (mapv
               (fn [i]
                 (mapv (fn [j] (fun i j)) (range width))
                 )
               (range height))]
    (->Mat height width (flatten data)))
  )

(defn make-identity [size]
  (make-mat-with-fun size size equal-1-else-0)
  )


(defn get-data [mat] (:data mat))

(defn mat-equal?
  ([a b] (mat-equal? a b EPSILON))
  ([a b tol] (and (= (:width a) (:width b))
                  (= (:height a) (:height b))
                  (equal? (get-data a) (get-data b) tol)))
  )

(defn m-get [mat i j]
  (nth (:data mat) (+ j (* i (:width mat)))))

(defn get-row [a i]
  (mapv (fn [j] (m-get a i j)) (range (:width a))))

(defn get-col [a j]
  (mapv (fn [i] (m-get a i j)) (range (:height a))))

(defn inner-dot [a b i j]
  (reduce +
          (mapv (fn [k] (* (m-get a i k) (m-get b k j)))
                (range (:width a))
                )
          )
  )

(defn m-dot [a b]
  (let [width (:width b)
        height (:height a)
        data (mapv
               (fn [i]
                 (mapv (fn [j] (inner-dot a b i j)) (range width))
                 )
               (range height))]
    (->Mat width height (flatten data))
    )
  )

(defn m-dot-vec [mat v]
  (:data (m-dot mat (->Mat (:width mat) 1 v)))
  )

(defn m-dot-vec-l [mat v]
  (:data (m-dot (->Mat 1 (:height mat) v) mat))
  )


(defn m-dot-with-copies [a b]
  (let [width (:width b)
        height (:height a)
        data (mapv
               (fn [i]
                 (m-dot-vec-l b (get-row a i))
                 )
               (range height))]
    (->Mat width height (flatten data))
    )
  )

(defn transpose [a]
  (make-mat-with-fun (:width a) (:height a) (fn [i j] (m-get a j i))))

(declare cofactor)
(defn det [a]
  (if (not (= (:width a) (:height a)))
    (throw (AssertionError. "Only square matrix determinant can be computed"))
    )
  (if (= (:width a) 2)
    (- (* (m-get a 0 0) (m-get a 1 1)) (* (m-get a 0 1) (m-get a 1 0)))
    (dot (get-row a 0) (mapv (fn [i] (cofactor a 0 i)) (range (:width a))))
    )
  )

(defn increment-if-lower [i thresh] (if (< i thresh) i (+ i 1)))

(defn del-row-col [mat delRow delCol]
  (make-mat-with-fun (- (:height mat) 1)
                     (- (:width mat) 1)
                     (fn [i j] (let [newI (increment-if-lower i delRow)
                              newJ (increment-if-lower j delCol)]
                                 (m-get mat newI newJ)))
                     ))

(defn minor [mat i j] (det (del-row-col mat i j)))

(defn cofactor [mat i j] (let [thisMinor (minor mat i j)]
                           (if (even? (+ i j))
                             thisMinor (- thisMinor))))

; invert is very slow for matrix of size higher than 7 or so
(defn invert [mat]
  (let [determinant (double (det mat))
        size (:width mat)]
    (if (== determinant 0)
      (throw (AssertionError. "Matrix is not invertible: det=0")))
    (make-mat-with-fun size size
                       (fn [i j] (/ (cofactor mat j i) determinant))))

  )


