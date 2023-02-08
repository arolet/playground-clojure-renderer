(ns renderer.objects
  (:require [renderer.matrix :refer [inverse mDotVec matEqual]]
            [renderer.ray :as Ray]
            [renderer.transformation :refer
             [chain rotationXYZ scaling sheering translation]]))

(defrecord Object3d [type intersect transformation inverted material])

(defn transformRay [mat {origin :origin direction :direction}]
  (Ray/makeRay (mDotVec mat origin) (mDotVec mat direction)))

(defrecord ObjIntersection [ray object3d point time])

(defn addObjectToHits [hits obj]
  (mapv (fn [inter] (assoc inter :object3d obj)) hits)
  )

(defn intersect [ray objects]
  (mapcat
    (fn [obj]
      (let [hits ((:intersect obj) (transformRay (:inverted obj) ray))]
        (addObjectToHits hits obj)
        )
      )
    objects)
  )

(defn objEqual [a b]
  (= (:type a) (:type b))
  (matEqual (:transformation a) (:transformation b))
  )

(defn makeSphere
  ([config] (makeSphere config [0 0 0]))
  ([config position] (makeSphere config position [0 0 0]))
  ([config position rotate] (makeSphere config position rotate [1 1 1]))
  ([config position rotate scale] (makeSphere config position rotate scale [0 0 0 0 0 0]))
  ([config position rotate scale sheer]
   (let [transformation (chain (apply sheering sheer)
                               (apply scaling scale)
                               (apply rotationXYZ rotate)
                               (apply translation position))]
     (->Object3d "sphere" Ray/intersectUnitSphere transformation (inverse transformation) config))
   )
  )
