(ns renderer.world
  (:require [renderer.objects :as Objects]
            [renderer.light :as Light]
            [renderer.ray :as Ray]
            [renderer.color :as Color]))

(defrecord World [objects light background])

(def default-background (Color/make-color 0.05 0.1 0.05))

(defn make-world ([] (make-world [] nil))
  ([objects light] (make-world objects light default-background))
  ([objects light background] (->World objects light background)))

(defn add-object [world object]
  (assoc world :objects (conj (:objects world) object)))

(defn intersect [world ray]
  (sort-by :time (Objects/intersect ray (:objects world))))

(defn shade-hit [{light :light} {obj :object3d point :point eyeV :eyeV normal :normal in-shadow? :in-shadow?}]
  (Light/phong-lighting light
                        (:material obj)
                        point
                        eyeV
                        normal
                        in-shadow?
                        ))

(defn color-at [world ray]
  (let [hits (intersect world ray)
        hit (Ray/hit hits)]
    (if (= nil hit)
      (:background world)
      (shade-hit world
                 (Light/compute-intersection-state hit world)))))
