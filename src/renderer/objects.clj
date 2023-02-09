(ns renderer.objects
  (:require [renderer.matrix :refer [inverse mDotVec matEqual transpose]]
            [renderer.ray :as Ray]
            [renderer.tuple :as Tuple]
            [renderer.transformation :refer
             [chain rotationXYZ scaling sheering translation]]
            [renderer.material :refer
             [material]]))

(defrecord Object3d [type intersect normalAt transformation inverted invertedTranspose material])

(defn transformRay [mat {origin :origin direction :direction}]
  (Ray/makeRay (mDotVec mat origin) (mDotVec mat direction)))

(defrecord ObjIntersection [ray object3d point time])

(defn addObjectToHits [hits obj ray]
  (mapv (fn [inter] (Ray/makeIntersection ray (:time inter) obj))
        hits)
  )

(defn intersect [ray objects]
  (mapcat
    (fn [obj]
      (let [hits ((:intersect obj) (transformRay (:inverted obj) ray))]
        (addObjectToHits hits obj ray)
        )
      )
    objects)
  )

(defn objEqual [a b]
  (= (:type a) (:type b))
  (matEqual (:transformation a) (:transformation b))
  )

(declare normalAtUnitSphere)

(defn makeSphereFromTransform [config transform]
  (let [inverted (inverse transform)]
    (->Object3d "sphere"
                Ray/intersectUnitSphere
                normalAtUnitSphere
                transform
                inverted
                (transpose inverted)
                config))
  )

(defn makeSphere
  ([] (makeSphere (material)))
  ([config] (makeSphere config [0 0 0]))
  ([config position] (makeSphere config position [0 0 0]))
  ([config position rotate] (makeSphere config position rotate [1 1 1]))
  ([config position rotate scale] (makeSphere config position rotate scale [0 0 0 0 0 0]))
  ([config position rotate scale sheer]
   (let [transformation (chain (apply sheering sheer)
                               (apply scaling scale)
                               (apply rotationXYZ rotate)
                               (apply translation position))]
     (makeSphereFromTransform config
                              transformation))
   )
  )


(def normalAtUnitSphere Tuple/castToVector)

(defn normalAt [obj pt]
  (let [ptOnUnitObj (mDotVec (:inverted obj) pt)
        normalOnUnit ((:normalAt obj) ptOnUnitObj)
        normalOnWorld (mDotVec (:invertedTranspose obj) normalOnUnit)
        normalOnWorldFixed (Tuple/castToVector normalOnWorld)]
    (Tuple/normalize normalOnWorldFixed))
  )
