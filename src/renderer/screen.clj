(ns renderer.screen
  (:require [renderer.tuple :as Tuple]
            [renderer.ray :as Ray]))

(defrecord Screen [origin vX vY])

(defrecord Camera [eye screen])

(defn make-camera [eye screen] (->Camera eye screen))

(defn make-screen [pxWidth pxHeight topLeft topRight bottomLeft]
  (let [eX (Tuple/div (Tuple/remove-tuple topRight topLeft) pxWidth)
        eY (Tuple/div (Tuple/remove-tuple bottomLeft topLeft) pxHeight)]
    (->Screen topLeft eX eY))
  )

(defn get-pixel-point [{origin :origin vX :vX vY :vY} col row]
  (Tuple/add-all origin
                 (Tuple/mul vX (+ col 0.5))
                 (Tuple/mul vY (+ row 0.5)))
  )

(defn get-pixel-ray [camera col row]
  (Ray/make-ray (:eye camera)
                (Tuple/cast-to-vector (get-pixel-point (:screen camera) col row))
                true))
