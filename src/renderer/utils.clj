(ns renderer.utils)

(defn closeTo ([a b] (closeTo a b 1e-8))
  ([a b tol] (< (abs (- a b)) tol)))
