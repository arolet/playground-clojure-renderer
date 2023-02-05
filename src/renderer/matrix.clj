(ns renderer.matrix
  (:require [renderer.tuple :refer [equal dot]]))

(defrecord Mat [width height data])

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

(defn mDotVec [mat v]
  (mapv (fn [i]
          (dot (getRow mat i) v))
        (range (:height mat)))
  )

(defn mDotVecL [mat v]
  (mapv (fn [j]
          (dot (getCol mat j) v))
        (range (:width mat)))
  )

(defn mDot [a b]
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
