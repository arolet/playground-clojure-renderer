(ns renderer.camera
  (:require [renderer.canvas :as Canvas]
            [renderer.matrix :as Mat]
            [renderer.ray :as Ray]
            [renderer.transformation :refer [chain translation]]
            [renderer.tuple :as Tuple]
            [renderer.world :refer [color-at]]))

(defrecord Camera
  [h-size v-size fov transform transform-inverse aspect px-size half-width half-height])

(defn get-camera-view-params [h-size v-size fov]
  (let [aspect (/ h-size v-size)
        half-view-length (Math/tan (/ fov 2))]
    (if (> aspect 1)
      [aspect half-view-length (/ half-view-length aspect)]
      [aspect (* half-view-length aspect) half-view-length])))

(defn make-camera
  ([h-size v-size fov] (make-camera h-size v-size fov (Mat/make-identity 4)))
  ([h-size v-size fov transform]
   (let [[aspect half-view-width half-view-height] (get-camera-view-params h-size v-size fov)]
     (->Camera h-size
               v-size
               fov
               transform
               (Mat/invert transform)
               aspect
               (* 2 (/ half-view-width h-size))
               half-view-width
               half-view-height))))


(defn view-transform [from to up]
  (let [forward (Tuple/normalize (Tuple/remove-tuple to from))
        up-n (Tuple/normalize up)
        left (Tuple/cross forward up-n)
        true-up (Tuple/cross left forward)
        orientation (Mat/->Mat 4
                               4
                               [(nth left 0) (nth left 1) (nth left 2) 0
                                (nth true-up 0) (nth true-up 1) (nth true-up 2) 0
                                (- (nth forward 0)) (- (nth forward 1)) (- (nth forward 2)) 0
                                0 0 0 1])]
    (chain (translation (- (nth from 0))
                        (- (nth from 1))
                        (- (nth from 2)))
           orientation)))

(defn ray-for-pixel [cam col row]
  (let [origin (Tuple/make-point 0 0 0)
        h-offset (- (:half-width cam) (* (+ 0.5 col) (:px-size cam)))
        v-offset (- (:half-height cam) (* (+ 0.5 row) (:px-size cam)))
        px-point (Tuple/make-point h-offset v-offset -1)
        local-ray (Ray/make-ray origin (Tuple/remove-tuple px-point origin))]
    (Ray/transform-ray (:transform-inverse cam)
                       local-ray
                       true)))

(defn pixel-at [world cam col row]
  (let [ray (ray-for-pixel cam col row)]
    (color-at world ray)))

(defn get-drawing-fun [world cam]
  (fn [row col] (pixel-at world cam col row)))

(defn render
  ([world cam] (render world cam nil))
  ([world cam fName] (render world cam fName false))
  ([world cam fName antialias]
   (let [canvas (if antialias
                  (Canvas/fn->canvas-antialias (get-drawing-fun world cam)
                                               (:h-size cam)
                                               (:v-size cam)
                                               fName)
                  (Canvas/fn->canvas (get-drawing-fun world cam)
                                     (:h-size cam)
                                     (:v-size cam)
                                     fName))]
     (if (nil? fName) canvas nil))))
