(ns renderer.scripts.draw_spheres
  (:require [renderer.camera :refer [make-camera render view-transform]]
            [renderer.texture.color :refer [make-color]]
            [renderer.texture.uniform :refer [uniform-texture]]
            [renderer.light :as Light]
            [renderer.material :refer [make-material]]
            [renderer.objects.factory :refer [make-object sphere-kw]]
            [renderer.tuple]
            [renderer.tuple :refer [make-point make-vector]]
            [renderer.utils :refer [combine-file-names]]
            [renderer.world :as World]))

(def background (make-color 0.01 0.05 0.1))
(def red-material (make-material (uniform-texture (make-color 0.9 0.3 0.05))))
(def blue-material (make-material (uniform-texture (make-color 0.05 0.3 0.8))))
(def yellow-material (make-material (uniform-texture (make-color 1 1 0.8))))

(def spheres [(make-object sphere-kw red-material [2 0 0] [0.5 0 0] [1.25 1.25 1.25])
              (make-object sphere-kw blue-material [1 -0.4 1] [0.5 -0.3 0] [0.33 0.25 6])
              (make-object sphere-kw yellow-material [1 0.4 0] [0 0 0] [0.1 0.1 4])])

(def light (Light/make-point-light (make-point -1 -1.3 -0.8)))

(def world (World/make-world spheres light background))

(def eye (make-point 0 0 0))
(def look-at (make-point 1 0 0))
(def up (make-vector 0 1 0))

(def cam (make-camera 100 100 (/ Math/PI 2) (view-transform eye look-at up)))

(def fName (combine-file-names "output" "sphere.ppm"))

(render world cam fName false)
