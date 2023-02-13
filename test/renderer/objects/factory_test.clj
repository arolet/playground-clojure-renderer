(ns renderer.objects.factory_test
  (:require [clojure.test :refer :all]
            [renderer.constants :refer :all]
            [renderer.matrix :refer [make-identity mat-equal?]]
            [renderer.objects.factory :refer :all]
            [renderer.objects.objects :refer :all]))

(deftest test-make-object
  (is (= :sphere (:type (:config (make-object :sphere)))))
  (is (mat-equal? (make-identity 4) (get-transformation (make-object :sphere)))))