(ns renderer.transformation
  (:require [renderer.matrix :refer [->Mat mDot]]))

(defn translation [x y z] (->Mat 4 4 [1 0 0 x
                                      0 1 0 y
                                      0 0 1 z
                                      0 0 0 1]))

(defn scaling [x y z] (->Mat 4 4 [x 0 0 0
                                  0 y 0 0
                                  0 0 z 0
                                  0 0 0 1]))

(defn rotationX [radians]
  (let [cosX (Math/cos radians)
        sinX (Math/sin radians)]
    (->Mat 4 4 [1 0 0 0
                0 cosX (- sinX) 0
                0 sinX cosX 0
                0 0 0 1]))
  )

(defn rotationY [radians]
  (let [cosY (Math/cos radians)
        sinY (Math/sin radians)]
    (->Mat 4 4 [cosY 0 sinY 0
                0 1 0 0
                (- sinY) 0 cosY 0
                0 0 0 1]))
  )

(defn rotationZ [radians]
  (let [cosZ (Math/cos radians)
        sinZ (Math/sin radians)]
    (->Mat 4 4 [cosZ (- sinZ) 0 0
                sinZ cosZ 0 0
                0 0 1 0
                0 0 0 1]))
  )

(defn sheering [xY xZ yX yZ zX zY]
  (->Mat 4 4 [1 xY xZ 0
              yX 1 yZ 0
              zX zY 1 0
              0 0 0 1]))

(defn chain [& transforms] (reduce mDot (reverse transforms)))

(defn rotationXYZ [x y z] (chain (rotationX x) (rotationY y) (rotationZ z)))
