(ns renderer.camera
  (:require [renderer.tuple :as Tuple]
            [renderer.ray :as Ray]
            [renderer.matrix :as Mat]
            [renderer.transformation :refer [chain translation]]))

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

(defn view-transform [from to up]
  (let [forward (Tuple/normalize (Tuple/remove-tuple to from))
        up-n (Tuple/normalize up)
        left (Tuple/cross forward up-n)
        true-up (Tuple/cross left forward)
        orientation (Mat/->Mat 4
                               4
                               [(nth left 0) (nth left 1) (nth left 2) 0
                                (nth true-up 0) (nth true-up 1) (nth true-up 2) 0
                                (- (nth forward 0)) (- (nth forward 1)) (- (nth forward 2)) 0
                                0 0 0 1])]
    (chain (translation (- (nth from 0))
                        (- (nth from 1))
                        (- (nth from 2)))
           orientation)
    )
  )
