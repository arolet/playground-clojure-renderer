(ns renderer.objects
  (:require [renderer.matrix :refer [inverse mDotVec matEqual]]
            [renderer.ray :as Ray]
            [renderer.transformation :refer
             [chain rotationXYZ scaling sheering translation]]))

(defrecord Object3d [type intersect transformation inverted])

(defn transformRay [mat {origin :origin direction :direction}]
  (Ray/makeRay (mDotVec mat origin) (mDotVec mat direction)))

(defrecord ObjIntersection [ray object3d point time])

(defn intersect [ray objects]
  (flatten
    (mapv
      (fn [obj]
        (mapv
          (fn [inter] (assoc inter :object3d obj))
          ((:intersect obj) (transformRay (:inverted obj) ray))))
      objects)
    )
  )

(defn objEqual [a b]
  (= (:type a) (:type b))
  (matEqual (:transformation a) (:transformation b))
  )

(defn makeSphere
  ([] (makeSphere [0 0 0]))
  ([position] (makeSphere position [0 0 0]))
  ([position rotate] (makeSphere position rotate [1 1 1]))
  ([position rotate scale] (makeSphere position rotate scale [0 0 0 0 0 0]))
  ([position rotate scale sheer]
   (let [transformation (chain (apply sheering sheer)
                               (apply scaling scale)
                               (apply rotationXYZ rotate)
                               (apply translation position))]
     (->Object3d "sphere" Ray/intersectUnitSphere transformation (inverse transformation)))
   )
  )
