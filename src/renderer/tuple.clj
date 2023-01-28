(ns renderer.tuple)

(defrecord Tuple [x y z w])

(defn makeVector [x y z] (->Tuple x y z 0.0))

(defn makePoint [x y z] (->Tuple x y z 1.0))

(defn isVector [tuple] (== (:w tuple) 0.0))

(defn isPoint [tuple] (== (:w tuple) 1.0))

(defn equal [a b]
  (reduce (fn [ncoll v]
            (and ncoll (== (v a) (v b))))
          true
          [:x :y :z :w]
          ))

(defn add [a b]
  (def merged (merge-with + a b))
  (assoc merged :w (min 1 (:w merged)))
  )

(defn removeTuple [a b] (merge-with - a b))

(defn mul [a factor]
  (def merged (reduce (fn [ncoll [k v]]
                        (assoc ncoll k (* factor v)))
                      {}
                      a))
  (assoc merged :w (:w a))
  )

(defn div [a factor]
  (mul a (/ 1. factor)))

(defn minus [a]
  (mul a -1))

(defn dot [a b]
  (reduce (fn [ncoll v]
            (+ ncoll (* (v a) (v b))))
          0
          [:x :y :z :w]
          ))

(defn norm [a]
  (Math/sqrt
    (reduce (fn [ncoll v]
              (def val (v a))
              (+ ncoll (* val val)))
            0
            [:x :y :z]
            )))

(defn cross [a b]
  (makeVector (- (* (:y a) (:z b)) (* (:z a) (:y b)))
              (- (* (:z a) (:x b)) (* (:x a) (:z b)))
              (- (* (:x a) (:y b)) (* (:y a) (:x b)))
              )
  )

(defn normalize [a]
  (div a (norm a)))
