(ns renderer.material
  (:require [renderer.texture.color :refer [make-color]]
            [renderer.texture.uniform :refer [uniform-texture]]))

(defrecord Material [texture ambient diffuse specular shininess])

(defn make-material
  ([] (make-material (uniform-texture (make-color 1 1 1))))
  ([texture] (make-material texture 0.1))
  ([texture ambient] (make-material texture ambient 0.9))
  ([texture ambient diffuse] (make-material texture ambient diffuse 0.9))
  ([texture ambient diffuse specular] (make-material texture ambient diffuse specular 200.0))
  ([texture ambient diffuse specular shininess] (->Material texture ambient diffuse specular shininess))
  )
