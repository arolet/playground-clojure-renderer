(ns renderer.texture.stripes
  (:require [renderer.matrix :refer [make-identity]]
            [renderer.texture.color :refer [make-color]]
            [renderer.texture.texture :refer [make-texture]]))

(defn in-stripe? ([pt]
                  (in-stripe? pt 0))
  ([pt index]
   (let [coord (nth pt index)]
     (< (mod coord 2) 1))))

(defn stripe-fun [pt on off]
  (if (in-stripe? pt)
    on
    off))

(defn stripes
  ([] (stripes (make-identity 4)))
  ([transform] (stripes transform false))
  ([transform local-coordinates?] (stripes transform local-coordinates? (make-color 1 1 1) (make-color 0 0 0)))
  ([transform local-coordinates? on off]
   (make-texture
     (fn [pt]
       (stripe-fun pt on off))
     transform
     local-coordinates?)))

(defn in-check? [pt]
  (= (in-stripe? pt 0) (in-stripe? pt 2)))

(defn checker-fun [pt on off]
  (if (in-check? pt)
    on
    off))

(defn checker
  ([] (checker (make-identity 4)))
  ([transform] (checker transform false))
  ([transform local-coordinates?] (checker transform local-coordinates? (make-color 1 1 1) (make-color 0 0 0)))
  ([transform local-coordinates? on off]
   (make-texture (fn [pt] (checker-fun pt on off))
                 transform
                 local-coordinates?)))
