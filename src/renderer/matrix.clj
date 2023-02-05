(ns renderer.matrix
  (:require [renderer.tuple :refer [equal dot]]))

(defrecord Mat [height width data])

(defn equal1else0 [a b]
  (if (== a b) 1 0))

(defn matFromFun [height width fun]
  (let [data (mapv
               (fn [i]
                 (mapv (fn [j] (fun i j)) (range width))
                 )
               (range height))]
    (->Mat height width (flatten data)))
  )

(defn makeIdentity [size]
  (matFromFun size size equal1else0)
  )


(defn getMatData [mat] (:data mat))

(defn matEqual [a b]
  (and (= (:width a) (:width b))
       (= (:height a) (:height b))
       (equal (getMatData a) (getMatData b)))
  )

(defn mGet [mat i j]
  (nth (:data mat) (+ j (* i (:width mat)))))

(defn getRow [a i]
  (mapv (fn [j] (mGet a i j)) (range (:width a))))

(defn getCol [a j]
  (mapv (fn [i] (mGet a i j)) (range (:height a))))

(defn innerDot [a b i j]
  (reduce +
          (mapv (fn [k] (* (mGet a i k) (mGet b k j)))
                (range (:width a))
                )
          )
  )

(defn mDot [a b]
  (let [width (:width b)
        height (:height a)
        data (mapv
               (fn [i]
                 (mapv (fn [j] (innerDot a b i j)) (range width))
                 )
               (range height))]
    (->Mat width height (flatten data))
    )
  )

(defn mDotVec [mat v]
  (:data (mDot mat (->Mat (:width mat) 1 v)))
  )

(defn mDotVecL [mat v]
  (:data (mDot (->Mat 1 (:height mat) v) mat))
  )


(defn mDotWithCopies [a b]
  (let [width (:width b)
        height (:height a)
        data (mapv
               (fn [i]
                 (mDotVecL b (getRow a i))
                 )
               (range height))]
    (->Mat width height (flatten data))
    )
  )

(defn transpose [a]
  (matFromFun (:width a) (:height a) (fn [i j] (mGet a j i))))

(declare cofactor)
(defn det [a]
  (if (not (= (:width a) (:height a)))
    (throw (AssertionError. "Only 2x2 mat determinant is implemented"))
    )
  (if (= (:width a) 2)
    (- (* (mGet a 0 0) (mGet a 1 1)) (* (mGet a 0 1) (mGet a 1 0)))
    (dot (getRow a 0) (mapv (fn [i] (cofactor a 0 i)) (range (:width a))))
    )
  )

(defn decrementIfLower [i thresh] (if (< i thresh) i (+ i 1)))
(defn delRowCol [mat delRow delCol]
  (matFromFun (- (:height mat) 1)
              (- (:width mat) 1)
              (fn [i j] (let [newI (decrementIfLower i delRow)
                              newJ (decrementIfLower j delCol)]
                          (mGet mat newI newJ)))
              ))

(defn minor [mat i j] (det (delRowCol mat i j)))

(defn cofactor [mat i j] (let [thisMinor (minor mat i j)]
                           (if (even? (+ i j))
                             thisMinor (- thisMinor))))
