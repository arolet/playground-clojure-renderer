(ns renderer.objects.factory
  (:require [renderer.material :refer [make-material]]
            [renderer.objects.objects :refer [->Object3d make-object-config]]
            [renderer.objects.sphere :as Sphere]
            [renderer.transformation :refer
             [chain rotation-xyz scaling sheering translation]]))

(def sphere-kw Sphere/sphere-kw)

(defn make-object-from-transform
  ([type transform] (make-object-from-transform type transform (make-material)))
  ([type transform material]
   (let [[intersect normal-at] (if (= type sphere-kw)
                       (Sphere/get-sphere-intersect-normal-at)
                       (throw (AssertionError. (format "Unknown object type %s" type))))]
     (->Object3d (make-object-config sphere-kw transform material)
                 intersect
                 normal-at))))

(defn make-object
  ([type] (make-object type (make-material)))
  ([type material] (make-object type material [0 0 0]))
  ([type material position] (make-object type material position [0 0 0]))
  ([type material position rotate] (make-object type material position rotate [1 1 1]))
  ([type material position rotate scale] (make-object type material position rotate scale [0 0 0 0 0 0]))
  ([type material position rotate scale sheer]
   (let [transform (chain (apply sheering sheer)
                               (apply scaling scale)
                               (apply rotation-xyz rotate)
                               (apply translation position))]
     (make-object-from-transform type transform material))))
