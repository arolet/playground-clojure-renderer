(ns renderer.light
  (:require [renderer.color :refer [makeColor elemMul]]
            [renderer.tuple :refer [mul normalize removeTuple dot addAll minus reflect]]))

(defrecord PointLight [intensity position])

(defn pointLight
  ([position] (pointLight position (makeColor 1 1 1)))
  ([position intensity] (->PointLight intensity position)))

(defn effectiveColor [light material] (elemMul (:intensity light) (:color material)))

(defn phongAmbient [effective material]
  (mul effective (:ambient material))
  )

(defn phongDiffuse [effective material eyeLightCos]
  (mul effective (* (:diffuse material) eyeLightCos))
  )

(defn phongSpecular [light material toLight toEye normal]
  (let [lightReflected (reflect (minus toLight) normal)
        lightToEyeCosine (dot lightReflected toEye)]
    (if (< lightToEyeCosine 0)
      (makeColor 0 0 0)
      (mul (:intensity light)
           (* (:specular material)
              (Math/pow lightToEyeCosine (:shininess material))))
      ))
  )

(defn phongLighting [light material point toEye normal]
  (let [effective (effectiveColor light material)
        ambient (phongAmbient effective material)
        toLight (normalize (removeTuple (:position light) point))
        eyeLightCos (dot toLight normal)]
    (if (< eyeLightCos 0)
      ambient
      (addAll ambient
         (phongDiffuse effective material eyeLightCos)
         (phongSpecular light material toLight toEye normal))
      )
    )
  )
