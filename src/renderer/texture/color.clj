(ns renderer.texture.color
  (:require [renderer.tuple :as Tuple]))

(def add Tuple/add)
(def mul Tuple/mul)
(def remove-color Tuple/remove-tuple)
(def equal Tuple/equal?)

(defn make-color [r g b] (identity [r g b]))

(defn elem-mul [a b]
  (mapv * a b))
