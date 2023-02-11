(ns renderer.scripts.draw_spheres
  (:require [renderer.canvas :as Canvas]
            [renderer.color :as Color]
            [renderer.light :as Light]
            [renderer.material :refer [make-material]]
            [renderer.objects :as Objects]
            [renderer.screen :refer [get-pixel-ray make-camera make-screen]]
            [renderer.tuple]
            [renderer.tuple :refer [make-point]]
            [renderer.utils :refer [combine-file-names]]
            [renderer.world :as World]))

(def background (Color/make-color 0.01 0.05 0.1))
(def red-material (make-material (Color/make-color 0.9 0.3 0.05)))
(def blue-material (make-material (Color/make-color 0.05 0.3 0.8)))
(def yellow-material (make-material (Color/make-color 1 1 0.8)))


(def antialiasing false)

(defn get-drawing-function [camera world]
  (fn [row col]
    (let [ray (get-pixel-ray camera col row)]
      (World/color-at world ray)
      )
    )
  )

(def to-canvas
  (if antialiasing
    Canvas/fn->canvas-antialias
    Canvas/fn->canvas))

(defn get-screen [width height] (make-screen width height (make-point 1 1 -1)
                                             (make-point 1 1 1)
                                             (make-point 1 -1 -1)))

(defn draw-world [eye width height world fName]
  (let [camera (make-camera eye (get-screen width height))]
    (to-canvas (get-drawing-function camera world)
               width
               height
               fName)
    )
  )

(def spheres [(Objects/make-sphere red-material [2 0 0] [0.5 0 0] [1.25 1.25 1.25])
              (Objects/make-sphere blue-material [1 -0.4 1] [0.5 -0.3 0] [0.33 0.25 6])
              (Objects/make-sphere yellow-material [1 0.4 0] [0 0 0] [0.1 0.1 4])
              ])


(def eye (make-point 0 0 0))

(def light (Light/make-point-light (make-point -1 -1.3 -0.8)))

(def world (World/make-world spheres light background))

(def fName (combine-file-names "output" "sphere.ppm"))

(def sphereCanvas (draw-world eye 250 250 world fName))

