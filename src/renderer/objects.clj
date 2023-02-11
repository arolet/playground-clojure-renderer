(ns renderer.objects
  (:require [renderer.matrix :refer [invert m-dot-vec mat-equal? transpose]]
            [renderer.ray :as Ray]
            [renderer.tuple :as Tuple]
            [renderer.transformation :refer
             [chain rotation-xyz scaling sheering translation]]
            [renderer.material :refer
             [make-material]]))

(defrecord Object3d [type intersect normalAt transformation inverted invertedTranspose material])

(defn transform-ray [mat {origin :origin direction :direction}]
  (Ray/make-ray (m-dot-vec mat origin) (m-dot-vec mat direction)))

(defrecord IntersectionState [time object3d point eyeV normal inside])

(defn add-object-to-hits [hits obj ray]
  (mapv (fn [inter] (Ray/make-intersection ray (:time inter) obj))
        hits)
  )

(defn intersect [ray objects]
  (mapcat
    (fn [obj]
      (let [hits ((:intersect obj) (transform-ray (:inverted obj) ray))]
        (add-object-to-hits hits obj ray)
        )
      )
    objects)
  )

(defn objEqual [a b]
  (= (:type a) (:type b))
  (mat-equal? (:transformation a) (:transformation b))
  )

(declare normal-at-unit-sphere intersect-unit-sphere)

(defn make-sphere-from-transform [config transform]
  (let [inverted (invert transform)]
    (->Object3d "sphere"
                intersect-unit-sphere
                normal-at-unit-sphere
                transform
                inverted
                (transpose inverted)
                config))
  )

(defn make-sphere
  ([] (make-sphere (make-material)))
  ([config] (make-sphere config [0 0 0]))
  ([config position] (make-sphere config position [0 0 0]))
  ([config position rotate] (make-sphere config position rotate [1 1 1]))
  ([config position rotate scale] (make-sphere config position rotate scale [0 0 0 0 0 0]))
  ([config position rotate scale sheer]
   (let [transformation (chain (apply sheering sheer)
                               (apply scaling scale)
                               (apply rotation-xyz rotate)
                               (apply translation position))]
     (make-sphere-from-transform config
                                 transformation))
   )
  )

(defn intersect-unit-sphere [ray]
  (let [[_projected dist t] (Ray/project-point ray (Tuple/make-point 0 0 0))]
    (if (or (> dist 1) (< t 0))
      []
      (let [distToIntersection (/ (Math/sqrt (- 1 (* dist dist))) (:norm ray))
            enterT (- t distToIntersection)
            leaveT (+ t distToIntersection)]
        [(Ray/make-intersection ray enterT)
         (Ray/make-intersection ray leaveT)])
      )
    )
  )



(def normal-at-unit-sphere Tuple/cast-to-vector)

(defn normal-at [obj pt]
  (let [ptOnUnitObj (m-dot-vec (:inverted obj) pt)
        normalOnUnit ((:normalAt obj) ptOnUnitObj)
        normalOnWorld (m-dot-vec (:invertedTranspose obj) normalOnUnit)
        normalOnWorldFixed (Tuple/cast-to-vector normalOnWorld)]
    (Tuple/normalize normalOnWorldFixed))
  )

(defn compute-intersection-state [intersection]
  (let [obj (:object3d intersection)
        point (Ray/get-point intersection)
        eyeV (Tuple/minus (:direction (:ray intersection)))
        normal (normal-at obj point)
        inside (< (Tuple/dot eyeV normal) 0)]
    (->IntersectionState (:time intersection)
                         obj
                         point
                         eyeV
                         (if inside (Tuple/minus normal) normal)
                         inside)
    )
  )
