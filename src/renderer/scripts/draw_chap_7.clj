(ns renderer.scripts.draw_chap_7
  (:require [renderer.camera :refer [make-camera render view-transform]]
            [renderer.color :as Color]
            [renderer.light :as Light]
            [renderer.material :refer [make-material]]
            [renderer.objects.factory :refer [make-object sphere-kw]]
            [renderer.tuple]
            [renderer.tuple :refer [make-point make-vector]]
            [renderer.utils :refer [combine-file-names]]
            [renderer.world :as World]))

(def background (Color/make-color 0.01 0.05 0.1))
(def floor-material (make-material (Color/make-color 1 0.9 0.9) 0.1 0.9 0))
(def green-material (make-material (Color/make-color 0.1 1 0.5) 0.1 0.7 0.3))
(def green2-material (make-material (Color/make-color 0.1 1 0.5) 0.1 0.7 0.3))
(def yellow-material (make-material (Color/make-color 1 0.8 0.1)))

(def spheres [(make-object sphere-kw floor-material [0 0 0] [0 0 0] [10 0.01 10])
              (make-object sphere-kw floor-material [0 0 5] [(/ Math/PI 2) (/ Math/PI -4) 0] [10 0.01 10])
              (make-object sphere-kw floor-material [0 0 5] [(/ Math/PI 2) (/ Math/PI 4) 0] [10 0.01 10])
              (make-object sphere-kw green-material [-0.5 1 0.1])
              (make-object sphere-kw green2-material [1.5 0.5 -0.5] [0 0 0] [0.5 0.5 0.5])
              (make-object sphere-kw yellow-material [-1.5 0.33 -0.75] [0 0 0] [0.33 0.33 0.33])])

(def light (Light/make-point-light (make-point -10 10 -10)))

(def world (World/make-world spheres light background))

(def eye (make-point 0 1.5 -5))
(def look-at (make-point 0 1 0))
(def up (make-vector 0 1 0))

(def cam (make-camera 500 250 (/ Math/PI 3) (view-transform eye look-at up)))

(def fName (combine-file-names "output" "chap_7.ppm"))

(render world cam fName true)

