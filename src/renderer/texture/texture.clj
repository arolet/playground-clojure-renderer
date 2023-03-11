(ns renderer.texture.texture
  (:require [renderer.objects.objects :refer [to-local]]
            [renderer.matrix :refer [m-dot-vec]]))

(defrecord Texture [fun use-transform? transform local-coordinates?])

(defn make-texture
  ([fun] (->Texture fun false nil false))
  ([fun transform] (make-texture fun transform false))
  ([fun transform local-coordinates?] (->Texture fun true transform local-coordinates?)))

(defn sample-texture
  ([text point]
   (if (contains? text :fun)
     (let [fun (:fun text)
           point (if (:use-transform? text)
                   (m-dot-vec (:transform text) point)
                   point)]
       (sample-texture (fun point) point))
     text))
  ([text obj point]
   (if (:local-coordinates? text)
     (sample-texture text (to-local obj point))
     (sample-texture text point))))
