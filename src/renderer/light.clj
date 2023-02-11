(ns renderer.light
  (:require [renderer.color :refer [make-color elem-mul]]
            [renderer.tuple :refer [mul normalize remove-tuple dot add-all minus reflect]]))

(defrecord PointLight [intensity position])

(defn make-point-light
  ([position] (make-point-light position (make-color 1 1 1)))
  ([position intensity] (->PointLight intensity position)))

(defn effective-color [light material] (elem-mul (:intensity light) (:color material)))

(defn phong-ambient [effective material]
  (mul effective (:ambient material))
  )

(defn phong-diffuse [effective material eyeLightCos]
  (mul effective (* (:diffuse material) eyeLightCos))
  )

(defn phong-specular [light material toLight toEye normal]
  (let [lightReflected (reflect (minus toLight) normal)
        lightToEyeCosine (dot lightReflected toEye)]
    (if (< lightToEyeCosine 0)
      (make-color 0 0 0)
      (mul (:intensity light)
           (* (:specular material)
              (Math/pow lightToEyeCosine (:shininess material))))
      ))
  )

(defn phong-lighting [light material point toEye normal]
  (let [effective (effective-color light material)
        ambient (phong-ambient effective material)
        toLight (normalize (remove-tuple (:position light) point))
        eyeLightCos (dot toLight normal)]
    (if (< eyeLightCos 0)
      ambient
      (add-all ambient
               (phong-diffuse effective material eyeLightCos)
               (phong-specular light material toLight toEye normal))
      )
    )
  )
