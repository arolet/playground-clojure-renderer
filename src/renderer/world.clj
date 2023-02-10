(ns renderer.world
  (:require [renderer.objects :as Objects]
            [renderer.light :as Light]
            [renderer.ray :as Ray]
            [renderer.color :as Color]))

(defrecord World [objects light background])

(def defaultBackground (Color/makeColor 0.05 0.1 0.05))

(defn world ([] (world [] nil))
  ([objects light] (world objects light defaultBackground))
  ([objects light background] (->World objects light background))
  )

(defn addObject [w object]
  (assoc w :objects (conj (:objects w) object))
  )

(defn intersect [w ray]
  (sort-by :time (Objects/intersect ray (:objects w)))
  )

(defn shadeHit [{light :light} {obj :object3d point :point eyeV :eyeV normal :normal}]
  (Light/phongLighting light
                       (:material obj)
                       point
                       eyeV
                       normal
                       )
  )

(defn colorAt [w ray]
  (let [hits (intersect w ray)
        hit (Ray/hit hits)]
    (if (= nil hit)
      (:background w)
      (shadeHit w
                (Objects/computeIntersectionState hit))
      )
    )
  )
