(ns renderer.objects.plane  (:require [renderer.ray :as Ray]
                                      [renderer.tuple :as Tuple]
                                      [renderer.utils :refer [close?]]))

(def plane-kw :plane)
(defn normal-on-y-plane [& _args] (Tuple/make-vector 0 1 0))

(def parallel-tol 1e-8)

(defn intersect-y-plane [ray]
  (let [y-dir (nth (:direction ray) 1)
        y-pos (nth (:origin ray) 1)]
    (if (close? 0 y-dir parallel-tol) [] [(Ray/make-intersection ray (- (/ y-pos y-dir)))])))

(defn get-plane-intersect-normal-at [] [intersect-y-plane normal-on-y-plane])
