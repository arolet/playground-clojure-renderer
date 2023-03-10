(ns renderer.material_test
  (:require [clojure.test :refer :all]
            [renderer.material :refer :all]
            [renderer.tuple :refer [equal?]]
            [renderer.texture.color :as Color]
            [renderer.texture.texture :refer [sample-texture]]))

(deftest test-make-material
  (let [defaultMaterial (make-material)]
    (is (equal? (Color/make-color 1 1 1) (sample-texture (:texture defaultMaterial) nil)))
    (is (== 0.1 (:ambient defaultMaterial)))
    (is (== 0.9 (:diffuse defaultMaterial)))
    (is (== 0.9 (:specular defaultMaterial)))
    (is (== 200 (:shininess defaultMaterial)))
    )
  )
