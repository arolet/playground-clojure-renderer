(ns renderer.utils
  (:require [clojure.java.io :as io]))

(defn closeTo ([a b] (closeTo a b 1e-8))
  ([a b tol] (< (abs (- a b)) tol)))

(defn combineFileNames [& args]
  (.getPath (apply io/file args)))

