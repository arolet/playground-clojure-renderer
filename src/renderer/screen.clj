(ns renderer.screen
  (:require [renderer.tuple :as Tuple]
            [renderer.ray :as Ray]))

(defrecord Screen [origin vX vY])

(defrecord Camera [eye screen])

(defn makeCamera [eye screen] (->Camera eye screen))

(defn makeScreen [pxWidth pxHeight topLeft topRight bottomLeft]
  (let [eX (Tuple/div (Tuple/removeTuple topRight topLeft) pxWidth)
        eY (Tuple/div (Tuple/removeTuple bottomLeft topLeft) pxHeight)]
    (->Screen topLeft eX eY))
  )

(defn getPixelPoint [{origin :origin vX :vX vY :vY} col row]
  (Tuple/addAll origin
                (Tuple/mul vX (+ col 0.5))
                (Tuple/mul vY (+ row 0.5)))
  )

(defn getPixelRay [camera col row]
  (Ray/makeRay (:eye camera)
               (Tuple/castToVector (getPixelPoint (:screen camera) col row))
               true))
