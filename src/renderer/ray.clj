(ns renderer.ray
  (:require [renderer.tuple :as Tuple]))


(defrecord Ray [origin direction norm])

(defrecord Intersection [ray time object3d])

(defn makeIntersection
  ([ray t] (makeIntersection ray t nil))
  ([ray t obj]
   (->Intersection ray
                   t
                   obj))
  )

(defn getPoint [{ray :ray t :time}]
  (Tuple/add (:origin ray) (Tuple/mul (:direction ray) t)))

(defn hit [intersections]
  (reduce
    (fn [red inter]
      (if (< (:time inter) 0)
        red
        (if (or (= nil red) (< (:time inter) (:time red)))
          inter
          red)
        )
      )
    nil
    intersections)
  )

(defn makeRay
  ([origin direction] (makeRay origin direction false))
  ([origin direction normalize]
   (if normalize
     (->Ray origin (Tuple/normalize direction) 1.0)
     (->Ray origin direction (Tuple/norm direction))
     )
   )
  )

(defn projectPoint [{origin :origin direction :direction norm :norm} point]
  (let [originToPoint (Tuple/removeTuple point origin)
        t (/ (Tuple/dot originToPoint direction) (* norm norm))
        projected (Tuple/add origin (Tuple/mul direction t))
        dist (Tuple/norm (Tuple/removeTuple point projected))]
    [projected dist t]
    )
  )

(defn intersectUnitSphere [ray]
  (let [[_projected dist t] (projectPoint ray (Tuple/makePoint 0 0 0))]
    (if (or (> dist 1) (< t 0))
      []
      (let [distToIntersection (/ (Math/sqrt (- 1 (* dist dist))) (:norm ray))
            enterT (- t distToIntersection)
            leaveT (+ t distToIntersection)]
        [(makeIntersection ray enterT)
         (makeIntersection ray leaveT)])
      )
    )
  )
