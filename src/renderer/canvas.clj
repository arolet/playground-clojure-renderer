(ns renderer.canvas
  (:require [renderer.color :as Color]
            [renderer.tuple :as Tuple]
            [clojure.string :as Strings]
            [clojure.math :refer [round]]))

(defrecord Canvas [data width height])

(defn colorCanvas [color width height]
  (->Canvas
    (vec (repeat height
                 (vec (repeat width color))))
    width
    height)
  )
(defn blankCanvas [width height]
  (colorCanvas (Color/makeColor 0 0 0)
               width
               height)
  )

(defn dataEqual [a b]
  (Tuple/equal (flatten a) (flatten b)))

(defn clip [a]
  (min 1 (max 0 a)))

(defn floatToInt [a]
  (mapv (fn [v] (round (* (clip v) 255))) a))

(defn equal [a b]
  (and (= (:width a) (:width b))
       (= (:height a) (:height b))
       (dataEqual (:data a) (:data b))
       )
  )

(declare fnToCanvasRow initPpm getRowSaveFunc)

(defn fnToCanvas
  ([func width height] (fnToCanvas func width height nil))
  ([func width height fName]
   (initPpm fName width height)
   (let [saveFunc (getRowSaveFunc fName)
         data (mapv
                (fn [row]
                  (fnToCanvasRow func row width saveFunc)
                  )
                (range height))]
     (->Canvas data width height)
     ))
  )

(defn fnToCanvasRow [pixelFun row width postFunc]
  (let [data (mapv (fn [col] (pixelFun row col)) (range width))]
    (postFunc data)
    data)
  )

(defn applyFunAntialiasing [func row col]
  (apply Tuple/average
         (mapv (fn [[rowOff colOff]] (func (+ row rowOff) (+ col colOff)))
               [[-0.25 -0.25] [0.25 -0.25] [-0.25 0.25] [0.25 0.25]])))

(defn fnToCanvasAntiAliased
  ([func width height] (fnToCanvasAntiAliased func width height nil))
  ([func width height fName] (let [antiAliasFun
        (fn [row col] (applyFunAntialiasing func row col))]
    (fnToCanvas antiAliasFun width height fName)
    ))
  )

(defn getSingleDataRow [row]
  (let [flat (flatten row)
        intRow (floatToInt flat)]
    (mapv (fn [val] (str val)) intRow)
    )
  )

(defn getDataRows [canvas]
  (mapv getSingleDataRow (:data canvas))
  )

(defn addValToPpmLine [{lines :lines current :current} val maxChars]
  (if (>= (+ (count current) (count val)) maxChars)
    {:lines   (conj lines (Strings/triml current))
     :current val}
    {:lines   lines
     :current (str current
                   (if (= current nil) "" " ")
                   val)}
    )
  )

(defn toPpmLine [row maxChars]
  (let [joined (reduce (fn [agg val] (addValToPpmLine agg val maxChars))
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


(defn toPpmLines [rows maxChars]
  (mapcat (fn [row] (toPpmLine row maxChars)) rows))


(defn getPpmHeader [width height]
  (format "P3\n%d %d\n255\n" width height)
  )

(defn toPpmString [canvas]
  (str (getPpmHeader (:width canvas) (:height canvas))
       (Strings/join "\n" (toPpmLines (getDataRows canvas) 70))
       "\n"
       ))

(defn saveAsPpm [fName canvas]
  (spit fName (toPpmString canvas)))

(defn initPpm [fName width height]
  (if (not (= fName nil))
    (spit fName (getPpmHeader width height)))
  )

(defn getRowSaveFunc [fName]
  (if (= fName nil)
    (fn [_row])
    (fn [row]
    (spit fName (str (Strings/join "\n" (toPpmLine (getSingleDataRow row) 70)) "\n") :append true)
    )
  )
  )
