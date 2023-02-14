(ns renderer.texture.uniform)

(defn uniform-texture [color]
  (fn [& _args] color))
