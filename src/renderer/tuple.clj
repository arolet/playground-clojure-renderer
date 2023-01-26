(ns renderer.tuple
    (:require [clojure.test :as test]))

(defrecord Tuple [x y z w])

(defn makeVector [x y z] (->Tuple x y z 0.0))

(defn makePoint [x y z] (->Tuple x y z 1.0))

(defn isVector [tuple] (== (:w tuple) 0.0))

(defn isPoint [tuple] (== (:w tuple) 1.0))
