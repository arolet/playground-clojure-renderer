(ns renderer.utils
  (:require [clojure.java.io :as io]))

(defn close? ([a b] (close? a b 1e-8))
  ([a b tol] (< (abs (- a b)) tol)))

(defn combine-file-names [& args]
  (.getPath (apply io/file args)))

