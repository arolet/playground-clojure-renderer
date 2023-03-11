(ns renderer.texture.uniform
  (:require [renderer.texture.texture :refer [make-texture]]))

(defn uniform-texture [color]
  (make-texture (fn [& _args] color)))
