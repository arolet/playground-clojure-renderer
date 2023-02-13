(ns renderer.light
  (:require [renderer.color :refer [make-color elem-mul]]
            [renderer.objects :refer [intersect normal-at]]
            [renderer.ray :refer [make-ray hit get-point]]
            [renderer.tuple :refer [add mul normalize remove-tuple dot add-all minus reflect norm]]))

(defrecord PointLight [intensity position])

(defn make-point-light
  ([position] (make-point-light position (make-color 1 1 1)))
  ([position intensity] (->PointLight intensity position)))

(defrecord IntersectionState [time object3d point eyeV normal inside in-shadow?])

(declare shadowed?)

(def acne-epsilon 1e-8)

(defn compute-intersection-state [intersection world]
  (let [obj (:object3d intersection)
        point (get-point intersection)
        eyeV (minus (:direction (:ray intersection)))
        normal (normal-at obj point)
        inside (< (dot eyeV normal) 0)
        normal-corrected (if inside (minus normal) normal)
        point-adjusted (add point (mul normal-corrected acne-epsilon))]
    (->IntersectionState (:time intersection)
                         obj
                         point-adjusted
                         eyeV
                         normal-corrected
                         inside
                         (shadowed? (:light world) (:objects world) point-adjusted))))

(defn effective-color [light material] (elem-mul (:intensity light) (:color material)))

(defn phong-ambient [effective material]
  (mul effective (:ambient material)))

(defn phong-diffuse [effective material eyeLightCos]
  (mul effective (* (:diffuse material) eyeLightCos)))

(defn phong-specular [light material toLight toEye normal]
  (let [lightReflected (reflect (minus toLight) normal)
        lightToEyeCosine (dot lightReflected toEye)]
    (if (< lightToEyeCosine 0)
      (make-color 0 0 0)
      (mul (:intensity light)
           (* (:specular material)
              (Math/pow lightToEyeCosine (:shininess material)))))))

(defn phong-lighting
  ([light material point toEye normal]
   (phong-lighting light material point toEye normal false))
  ([light material point toEye normal in-shadow?]
   (let [effective (effective-color light material)
         ambient (phong-ambient effective material)
         toLight (normalize (remove-tuple (:position light) point))
         eyeLightCos (dot toLight normal)]
     (if (or in-shadow? (<= eyeLightCos 0))
       ambient
       (add-all ambient
                (phong-diffuse effective material eyeLightCos)
                (phong-specular light material toLight toEye normal))))))

(defn hit-between-point-and-light [m-hit max-length]
  (and (not (nil? m-hit)) (< (:time m-hit) max-length)))

(defn shadowed? [light objects point]
  (let [point-to-light (remove-tuple (:position light) point)
        ray (make-ray point point-to-light true)
        intersections (intersect ray objects)
        m-hit (hit intersections)]
    (hit-between-point-and-light m-hit (norm point-to-light))))
