(ns renderer.tuple)

(defrecord Tuple [x y z w])

(defn makeVector [x y z] (->Tuple x y z 0.0))

(defn makePoint [x y z] (->Tuple x y z 1.0))

(defn isVector [tuple] (== (:w tuple) 0.0))

(defn isPoint [tuple] (== (:w tuple) 1.0))

(defn add [a b] (->Tuple (+ (:x a) (:x b)) (+ (:y a) (:y b)) (+ (:z a) (:z b)) (- (+ (:w a) (:w b)) (* (:w a) (:w b)))))
