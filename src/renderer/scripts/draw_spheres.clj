(ns renderer.scripts.draw_spheres
  (:require [renderer.objects :as Objects]
            [renderer.canvas :as Canvas]
            [renderer.color :as Color]
            [renderer.ray :as Ray]
            [renderer.tuple :as Tuple]
            [renderer.tuple :refer [makePoint]]
            [renderer.screen :as Screen]))

(defn makeRay [origin pixelPoint]
  (Ray/makeRay origin (Tuple/removeTuple pixelPoint origin)))

(def black (Color/makeColor 0.01 0.05 0.1))
(def red (Color/makeColor 0.9 0.3 0.05))
(def blue (Color/makeColor 0.05 0.3 0.8))
(def yellow (Color/makeColor 1 1 0.8))

(defn computePixelHit [screen objects col row]
  (let [ray (makeRay (makePoint 0 0 0) (Screen/getPixelPoint screen col row))
        intersections (Objects/intersect ray objects)]
    (Ray/hit intersections))
  )

(defn getDrawingFunction [screen objects]
  (fn [row col]
    (let [hit (computePixelHit screen objects col row)]
      (if (= hit nil)
        black
        (:color (:material (:object3d hit)))))
    )
  )

(defn getScreen [width height] (Screen/makeScreen width height (makePoint 1 1 -1)
                                      (makePoint 1 1 1)
                                      (makePoint 1 -1 -1)))

(defn drawSpheres [width height objects]
  (let [screen (getScreen width height)]
    (Canvas/fnToCanvasAntiAliased (getDrawingFunction screen objects)
                                  width
                                  height)
    )
  )

(def spheres [(Objects/makeSphere {:color red} [2 0 0] [0.5 0 0] [1.25 1.25 1.25])
              (Objects/makeSphere {:color blue} [1 -0.4 1] [0.5 -0.3 0] [0.33 0.25 6])
              (Objects/makeSphere {:color yellow} [1 0.4 0] [0 0 0] [0.1 0.1 4])
              ])

(def sphereCanvas (drawSpheres 250 250 spheres))

(Canvas/saveAsPpm "sphere.ppm" sphereCanvas)
