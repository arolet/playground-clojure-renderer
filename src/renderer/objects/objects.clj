(ns renderer.objects.objects
  (:require [renderer.matrix :refer [invert m-dot-vec mat-equal? transpose]]
            [renderer.ray :as Ray]
            [renderer.tuple :as Tuple]))

(defrecord ObjectConfig [type transformation inverted inverted-transpose material])

(defrecord Object3d [config intersect normalAt])

(defn make-object-config [type transformation material]
  (let [inverted (invert transformation)]
    (->ObjectConfig type
                    transformation
                    inverted
                    (transpose inverted)
                    material)))

(defn get-material [obj]
  (:material (:config obj)))

(defn get-transformation [obj]
  (:transformation (:config obj)))

(defn get-inverted [obj]
  (:inverted (:config obj)))

(defn get-inverted-transpose [obj]
  (:inverted-transpose (:config obj)))

(defn to-local [obj pt]
  (m-dot-vec (get-inverted obj) pt))

(defn add-object-to-hits [hits obj ray]
  (mapv (fn [inter] (Ray/make-intersection ray (:time inter) obj))
        hits))

(defn intersect [ray objects]
  (mapcat
    (fn [obj]
      (let [hits ((:intersect obj) (Ray/transform-ray (get-inverted obj) ray))]
        (add-object-to-hits hits obj ray)))
    objects))

(defn objEqual [a b]
  (= (:type a) (:type b))
  (mat-equal? (:transformation a) (:transformation b)))

(defn normal-at [obj pt]
  (let [ptOnUnitObj (m-dot-vec (get-inverted obj) pt)
        normalOnUnit ((:normalAt obj) ptOnUnitObj)
        normalOnWorld (m-dot-vec (get-inverted-transpose obj) normalOnUnit)
        normalOnWorldFixed (Tuple/cast-to-vector normalOnWorld)]
    (Tuple/normalize normalOnWorldFixed)))
