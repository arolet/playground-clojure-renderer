(ns renderer.screen
  (:require [renderer.tuple :as Tuple]))

(defrecord Screen [origin vX vY])

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
