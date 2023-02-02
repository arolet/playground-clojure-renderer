(ns renderer.color
  (:require [renderer.tuple :as Tuple]))

(def add Tuple/add)
(def mul Tuple/mul)
(def removeColor Tuple/removeTuple)
(def equal Tuple/equal)

(defrecord Color [r g b])

(defn elemMul [a b]
  (merge-with * a b))
