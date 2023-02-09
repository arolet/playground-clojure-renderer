(ns renderer.material
  (:require [renderer.color :refer [makeColor]])
  )

(defrecord Material [color ambient diffuse specular shininess])

(defn material
  ([] (material (makeColor 1 1 1)))
  ([color] (material color 0.1))
  ([color ambient] (material color ambient 0.9))
  ([color ambient diffuse] (material color ambient diffuse 0.9))
  ([color ambient diffuse specular] (material color ambient diffuse specular 200.0))
  ([color ambient diffuse specular shininess] (->Material color ambient diffuse specular shininess))
  )
