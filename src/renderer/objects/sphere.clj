(ns renderer.objects.sphere
  (:require [renderer.ray :as Ray]
            [renderer.tuple :as Tuple]))

(def sphere-kw :sphere)
(declare intersect-unit-sphere)
(def normal-at-unit-sphere Tuple/cast-to-vector)

(defn intersect-unit-sphere [ray]
  (let [[_projected dist t] (Ray/project-point ray (Tuple/make-point 0 0 0))]
    (if (or (> dist 1) (< t 0))
      []
      (let [distToIntersection (/ (Math/sqrt (- 1 (* dist dist))) (:norm ray))
            enterT (- t distToIntersection)
            leaveT (+ t distToIntersection)]
        [(Ray/make-intersection ray enterT)
         (Ray/make-intersection ray leaveT)]))))

(defn get-sphere-intersect-normal-at [] [intersect-unit-sphere normal-at-unit-sphere])
