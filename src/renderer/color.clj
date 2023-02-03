(ns renderer.color
  (:require [renderer.tuple :as Tuple]))

(def add Tuple/add)
(def mul Tuple/mul)
(def removeColor Tuple/removeTuple)
(def equal Tuple/equal)

(defn makeColor [r g b] (identity [r g b]))

(defn elemMul [a b]
  (mapv * a b))
