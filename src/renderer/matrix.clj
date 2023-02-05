(ns renderer.matrix
  (:require [renderer.tuple :refer [equal dot]]))

(defrecord Mat [height width data])

(defn equal1else0 [a b]
  (if (== a b) 1 0))

(defn makeFromFun [height width fun]
  (let [data (mapv
               (fn [i]
                 (mapv (fn [j] (fun i j)) (range width))
                 )
               (range height))]
    (->Mat height width (flatten data)))
  )

(defn makeIdentity [size]
  (makeFromFun size size equal1else0)
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
  (makeFromFun (:width a) (:height a) (fn [i j] (mGet a j i))))
