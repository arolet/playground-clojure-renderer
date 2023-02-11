(ns renderer.tuple)

(defn make-vector [x y z] (identity [x y z 0.0]))

(defn make-point [x y z] (identity [x y z 1.0]))

(defn tuple-vector? [tuple] (== (nth tuple 3) 0.0))

(defn point? [tuple] (== (nth tuple 3) 1.0))

(def EPSILON 1e-6)

(defn equal?
  ([a b] (equal? a b EPSILON))
  ([a b tol] (and (= (count a) (count b))
                  (reduce (fn [agg v]
                            (and agg (<= (abs v) tol))
                            )
                          true
                          (mapv - a b))
                  ))
  )

(defn add [a b] (mapv + a b))

(defn remove-tuple [a b] (mapv - a b))

(defn mul [a factor]
  (mapv (fn [v] (* factor v)) a)
  )
(defn point-mul [a factor]
  (assoc (mapv (fn [v] (* factor v)) a) 3 (nth a 3))
  )

(defn div [a factor]
  (mul a (/ 1. factor)))
(defn point-iv [a factor]
  (point-mul a (/ 1. factor)))

(defn minus [a]
  (mul a -1))
(defn pointMinus [a]
  (point-mul a -1))

(defn dot [a b]
  (reduce + 0 (mapv * a b))
  )

(defn norm [a]
  (Math/sqrt (dot a a)))

(defn point-norm [a]
  (norm (subvec a 0 (- (count a) 1)))
  )

(defn cross [a b]
  (make-vector (- (* (nth a 1) (nth b 2)) (* (nth a 2) (nth b 1)))
               (- (* (nth a 2) (nth b 0)) (* (nth a 0) (nth b 2)))
               (- (* (nth a 0) (nth b 1)) (* (nth a 1) (nth b 0)))
               )
  )

(defn normalize [a]
  (div a (norm a)))

(defn same-direction?
  ([a b] (same-direction? a b EPSILON))
  ([a b tol]
   (>= (dot a b) (- (* (norm a) (norm b)) tol)))
  )

(defn add-all [& tuples]
  (reduce add tuples)
  )

(defn average [& tuples]
  ; don't use on points
  (div (apply add-all tuples) (count tuples))
  )

(defn reflect [in reference]
  (add in (mul reference (* -2 (dot in reference))))
  )

(defn cast-to-vector [pt]
  (make-vector (nth pt 0) (nth pt 1) (nth pt 2))
  )
