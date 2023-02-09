(ns renderer.scripts.draw_spheres
  (:require [clojure.test :refer :all]
            [renderer.objects :as Objects]
            [renderer.canvas :as Canvas]
            [renderer.color :as Color]
            [renderer.ray :as Ray]
            [renderer.tuple :as Tuple]
            [renderer.tuple :refer [makePoint minus castToVector]]
            [renderer.screen :refer [makeScreen getPixelRay makeCamera]]
            [renderer.material :refer [material]]
            [renderer.light :as Light]))




(def background (Color/makeColor 0.01 0.05 0.1))
(def redMaterial (material (Color/makeColor 0.9 0.3 0.05)))
(def blueMaterial (material (Color/makeColor 0.05 0.3 0.8)))
(def yellowMaterial (material (Color/makeColor 1 1 0.8)))


(def antialiasing true)


(defn computePixelHit [camera objects col row]
  (let [ray (getPixelRay camera col row)
        intersections (Objects/intersect ray objects)]
    (Ray/hit intersections))
  )

(defn computeColor [hit light]
  (let [point (Ray/getPoint hit)
        obj (:object3d hit)]
    (Light/phongLighting light
                         (:material obj)
                         point
                         (minus (:direction (:ray hit)))
                         (Objects/normalAt obj point)
                         ))
  )

(defn getDrawingFunction [camera objects light]
  (fn [row col]
    (let [hit (computePixelHit camera objects col row)]
      (if (= hit nil)
        background
        (computeColor hit light)
        )
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

(defn drawSpheres [eye width height objects light]
  (let [camera (makeCamera eye (getScreen width height))]
    (toCanvas (getDrawingFunction camera objects light)
              width
              height)
    )
  )

(def spheres [(Objects/makeSphere redMaterial [2 0 0] [0.5 0 0] [1.25 1.25 1.25])
              (Objects/makeSphere blueMaterial [1 -0.4 1] [0.5 -0.3 0] [0.33 0.25 6])
              (Objects/makeSphere yellowMaterial [1 0.4 0] [0 0 0] [0.1 0.1 4])
              ])


(def eye (makePoint 0 0 0))

(def myLight (Light/pointLight (makePoint -1 -1.3 -0.8)))

(def sphereCanvas (drawSpheres eye 500 500 spheres myLight))

(Canvas/saveAsPpm "sphere.ppm" sphereCanvas)
