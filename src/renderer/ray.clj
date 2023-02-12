(ns renderer.ray
  (:require [renderer.matrix :refer [m-dot-vec]]
            [renderer.tuple :as Tuple]))


(defrecord Ray [origin direction norm])

(defn make-ray
  ([origin direction] (make-ray origin direction false))
  ([origin direction normalize]
   (if normalize
     (->Ray origin (Tuple/normalize direction) 1.0)
     (->Ray origin direction (Tuple/norm direction))
     )
   )
  )

(defn transform-ray
  ([mat ray] (transform-ray mat ray false))
  ([mat {origin :origin direction :direction} normalize]
   (make-ray (m-dot-vec mat origin) (m-dot-vec mat direction) normalize)))

(defrecord Intersection [ray time object3d])

(defn make-intersection
  ([ray t] (make-intersection ray t nil))
  ([ray t obj]
   (->Intersection ray
                   t
                   obj))
  )

(defn get-point [{ray :ray t :time}]
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

(defn project-point [{origin :origin direction :direction norm :norm} point]
  (let [originToPoint (Tuple/remove-tuple point origin)
        t (/ (Tuple/dot originToPoint direction) (* norm norm))
        projected (Tuple/add origin (Tuple/mul direction t))
        dist (Tuple/norm (Tuple/remove-tuple point projected))]
    [projected dist t]
    )
  )
