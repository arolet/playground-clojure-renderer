(ns renderer.scripts.draw_spheres
  (:require [renderer.canvas :as Canvas]
            [renderer.color :as Color]
            [renderer.light :as Light]
            [renderer.material :refer [material]]
            [renderer.objects :as Objects]
            [renderer.screen :refer [getPixelRay makeCamera makeScreen]]
            [renderer.tuple]
            [renderer.tuple :refer [makePoint]]
            [renderer.utils :refer [combineFileNames]]
            [renderer.world :as World]))

(def background (Color/makeColor 0.01 0.05 0.1))
(def redMaterial (material (Color/makeColor 0.9 0.3 0.05)))
(def blueMaterial (material (Color/makeColor 0.05 0.3 0.8)))
(def yellowMaterial (material (Color/makeColor 1 1 0.8)))


(def antialiasing false)

(defn getDrawingFunction [camera world]
  (fn [row col]
    (let [ray (getPixelRay camera col row)]
      (World/colorAt world ray)
      )
    )
  )

(def toCanvas
  (if antialiasing
    Canvas/fnToCanvasAntiAliased
    Canvas/fnToCanvas))

(defn getScreen [width height] (makeScreen width height (makePoint 1 1 -1)
                                           (makePoint 1 1 1)
                                           (makePoint 1 -1 -1)))

(defn drawWorld [eye width height world fName]
  (let [camera (makeCamera eye (getScreen width height))]
    (toCanvas (getDrawingFunction camera world)
              width
              height
              fName)
    )
  )

(def spheres [(Objects/makeSphere redMaterial [2 0 0] [0.5 0 0] [1.25 1.25 1.25])
              (Objects/makeSphere blueMaterial [1 -0.4 1] [0.5 -0.3 0] [0.33 0.25 6])
              (Objects/makeSphere yellowMaterial [1 0.4 0] [0 0 0] [0.1 0.1 4])
              ])


(def eye (makePoint 0 0 0))

(def myLight (Light/pointLight (makePoint -1 -1.3 -0.8)))

(def world (World/world spheres myLight background))

(def fName (combineFileNames "output" "sphere.ppm"))

(def sphereCanvas (drawWorld eye 250 250 world fName))

