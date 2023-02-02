(ns renderer.tuple)

(defrecord Tuple [x y z w])

(defn makeVector [x y z] (->Tuple x y z 0.0))

(defn makePoint [x y z] (->Tuple x y z 1.0))

(defn isVector [tuple] (== (:w tuple) 0.0))

(defn isPoint [tuple] (== (:w tuple) 1.0))

(defn allKeys [a b]
  (distinct (concat (keys a) (keys b)))
  )

(defn sameKeys [a b] (= (keys a) (keys b)))

(def EPSILON 1e-6)

(defn equal [a b]
  (and (sameKeys a b)
    (reduce (fn [agg v]
              (and agg (<= (Math/abs (- (v a) (v b))) EPSILON)))
            true
            (allKeys a b)
            )
    ))

(defn add [a b]
  (def merged (merge-with + a b))
  (if (contains? a :w)
    (assoc merged :w (min 1 (:w merged)))
    (identity merged))
  )

(defn removeTuple [a b] (merge-with - a b))

(defn mul [a factor]
  (def merged (reduce (fn [ncoll [k v]]
                        (assoc ncoll k (* factor v)))
                      {}
                      a))
  (if (contains? a :w) (assoc merged :w (:w a)) (identity merged))
  )

(defn div [a factor]
  (mul a (/ 1. factor)))

(defn minus [a]
  (mul a -1))

(defn dot [a b]
  (reduce (fn [agg v]
            (+ agg (* (v a) (v b))))
          0
          (allKeys a b)
          ))

(defn norm [a]
  (Math/sqrt
    (reduce (fn [agg v]
              (def coord (v a))
              (+ agg (* coord coord)))
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
