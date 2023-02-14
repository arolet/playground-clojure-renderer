(ns renderer.canvas
  (:require [clojure.math :refer [round]]
            [clojure.string :as Strings]
            [renderer.texture.color :as Color]
            [renderer.tuple :as Tuple]))

(defrecord Canvas [data width height])

(defn color-canvas [color width height]
  (->Canvas
    (vec (repeat height
                 (vec (repeat width color))))
    width
    height)
  )
(defn blank-canvas [width height]
  (color-canvas (Color/make-color 0 0 0)
                width
                height)
  )

(defn data-equal [a b]
  (Tuple/equal? (flatten a) (flatten b)))

(defn clip [a]
  (min 1 (max 0 a)))

(defn floats->ints [a]
  (mapv (fn [v] (round (* (clip v) 255))) a))

(defn equal [a b]
  (and (= (:width a) (:width b))
       (= (:height a) (:height b))
       (data-equal (:data a) (:data b))
       )
  )

(declare fn->canvas-row init-ppm get-row-save-fun)

(defn fn->canvas
  ([func width height] (fn->canvas func width height nil))
  ([func width height fName]
   (init-ppm fName width height)
   (let [saveFunc (get-row-save-fun fName)
         data (mapv
                (fn [row]
                  (fn->canvas-row func row width saveFunc)
                  )
                (range height))]
     (->Canvas data width height)
     ))
  )

(defn fn->canvas-row [pixelFun row width postFunc]
  (let [data (mapv (fn [col] (pixelFun row col)) (range width))]
    (postFunc data)
    )
  )

(defn compose-antialiasing [func row col]
  (apply Tuple/average
         (mapv (fn [[rowOff colOff]] (func (+ row rowOff) (+ col colOff)))
               [[-0.25 -0.25] [0.25 -0.25] [-0.25 0.25] [0.25 0.25]])))

(defn fn->canvas-antialias
  ([func width height] (fn->canvas-antialias func width height nil))
  ([func width height fName] (let [antiAliasFun
                                   (fn [row col] (compose-antialiasing func row col))]
                               (fn->canvas antiAliasFun width height fName)
                               ))
  )

(defn get-data-row [row]
  (let [flat (flatten row)
        intRow (floats->ints flat)]
    (mapv (fn [val] (str val)) intRow)
    )
  )

(defn get-all-data-rows [canvas]
  (mapv get-data-row (:data canvas))
  )

(defn add-val-to-ppm-line [{lines :lines current :current} val maxChars]
  (if (>= (+ (count current) (count val)) maxChars)
    {:lines   (conj lines (Strings/triml current))
     :current val}
    {:lines   lines
     :current (str current
                   (if (= current nil) "" " ")
                   val)}
    )
  )

(defn row->ppm-line [row maxChars]
  (let [joined (reduce (fn [agg val] (add-val-to-ppm-line agg val maxChars))
                       {:lines [] :current nil}
                       row)
        {last :current lines :lines} joined
        ]
    (if (= (count last) 0)
      lines
      (conj lines last)
      )
    )
  )


(defn rows->ppm-lines [rows maxChars]
  (mapcat (fn [row] (row->ppm-line row maxChars)) rows))


(defn get-ppm->header [width height]
  (format "P3\n%d %d\n255\n" width height)
  )

(defn canvas->ppm [canvas]
  (str (get-ppm->header (:width canvas) (:height canvas))
       (Strings/join "\n" (rows->ppm-lines (get-all-data-rows canvas) 70))
       "\n"
       ))

(defn save-as-ppm [fName canvas]
  (spit fName (canvas->ppm canvas)))

(defn init-ppm [fName width height]
  (if (not (= fName nil))
    (spit fName (get-ppm->header width height)))
  )

(defn get-row-save-fun [fName]
  (if (= fName nil)
    identity
    (fn [row]
      (spit fName (str (Strings/join "\n" (row->ppm-line (get-data-row row) 70)) "\n") :append true)
      )
    )
  )
