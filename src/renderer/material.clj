(ns renderer.material
  (:require [renderer.color :refer [make-color]])
  )

(defrecord Material [color ambient diffuse specular shininess])

(defn make-material
  ([] (make-material (make-color 1 1 1)))
  ([color] (make-material color 0.1))
  ([color ambient] (make-material color ambient 0.9))
  ([color ambient diffuse] (make-material color ambient diffuse 0.9))
  ([color ambient diffuse specular] (make-material color ambient diffuse specular 200.0))
  ([color ambient diffuse specular shininess] (->Material color ambient diffuse specular shininess))
  )
