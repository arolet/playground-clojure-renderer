(ns renderer.tuple)

(defn makeVector [x y z] (identity [x y z 0.0]))

(defn makePoint [x y z] (identity [x y z 1.0]))

(defn isVector [tuple] (== (nth tuple 3) 0.0))

(defn isPoint [tuple] (== (nth tuple 3) 1.0))

(def EPSILON 1e-6)

(defn equal [a b]
  (and (= (count a) (count b))
       (reduce (fn [agg v]
                 (and agg (<= (abs v) EPSILON))
                 )
               true
               (mapv - a b))
       )
  )

(defn add [a b] (mapv + a b))

(defn removeTuple [a b] (mapv - a b))

(defn mul [a factor]
  (mapv (fn [v] (* factor v)) a)
  )
(defn pointMul [a factor]
  (assoc (mapv (fn [v] (* factor v)) a) 3 (nth a 3))
  )

(defn div [a factor]
  (mul a (/ 1. factor)))
(defn pointDiv [a factor]
  (pointMul a (/ 1. factor)))

(defn minus [a]
  (mul a -1))
(defn pointMinus [a]
  (pointMul a -1))

(defn dot [a b]
  (reduce + 0 (mapv * a b))
  )

(defn norm [a]
  (Math/sqrt (dot a a)))

(defn pointNorm [a]
  (norm (subvec  a 0 (- (count a) 1)))
  )

(defn cross [a b]
  (makeVector (- (* (nth a 1) (nth b 2)) (* (nth a 2) (nth b 1)))
              (- (* (nth a 2) (nth b 0)) (* (nth a 0) (nth b 2)))
              (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0)))
              )
  )

(defn normalize [a]
  (div a (norm a)))
