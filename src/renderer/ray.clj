(ns renderer.ray
  (:require [renderer.tuple :as Tuple]))


(defrecord Ray [origin direction])

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
  (let [[projected dist t] (projectPoint ray (Tuple/makePoint 0 0 0))]
    (if (or (> dist 1) (< t 0))
      [nil nil]
      (let [distToIntersection (Math/sqrt (- 1 (* dist dist)))
            toAdd (Tuple/mul (:direction ray) distToIntersection)]
        [(Tuple/add projected (Tuple/minus toAdd))
         (Tuple/add projected toAdd)])
      )
    )
  )
