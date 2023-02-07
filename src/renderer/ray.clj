(ns renderer.ray
  (:require [renderer.tuple :as Tuple]))


(defrecord Ray [origin direction])

(defrecord Intersection [point time])

(defn makeIntersection [{origin :origin direction :direction} t]
  (->Intersection (Tuple/add origin (Tuple/mul direction t)) t))

(defn makeRay [origin direction]
  (->Ray origin (Tuple/normalize direction))
  )

(defn projectPoint [{origin :origin direction :direction} point]
  (let [originToPoint (Tuple/removeTuple point origin)
        t (Tuple/dot originToPoint direction)
        projected (Tuple/add origin (Tuple/mul direction t))
        dist (Tuple/norm (Tuple/removeTuple point projected))]
    [projected dist t]
    )
  )

(defn intersectUnitSphere [ray]
  (let [[_projected dist t] (projectPoint ray (Tuple/makePoint 0 0 0))]
    (if (or (> dist 1) (< t 0))
      []
      (let [distToIntersection (Math/sqrt (- 1 (* dist dist)))
            enterT (- t distToIntersection)
            leaveT (+ t distToIntersection)]
        [(makeIntersection ray enterT)
         (makeIntersection ray leaveT)])
      )
    )
  )
