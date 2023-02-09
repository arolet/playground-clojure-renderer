(ns renderer.material_test
  (:require [clojure.test :refer :all]
            [renderer.material :refer :all]
            [renderer.tuple :refer [equal]]
            [renderer.color :as Color]))

(deftest makeMaterial
  (let [defaultMaterial (material)]
    (is (equal (Color/makeColor 1 1 1) (:color defaultMaterial)))
    (is (== 0.1 (:ambient defaultMaterial)))
    (is (== 0.9 (:diffuse defaultMaterial)))
    (is (== 0.9 (:specular defaultMaterial)))
    (is (== 200 (:shininess defaultMaterial)))
    )
  )
