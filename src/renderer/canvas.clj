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

(defn fnToCanvas [func width height]
  (let [data (mapv
               (fn [row]
                 (mapv (fn [col] (func row col)) (range width))
                 )
               (range height))]
    (->Canvas data width height)
    )
  )

(defn applyFunAntialiasing [func row col]
  (apply Tuple/average
         (mapv (fn [[rowOff colOff]] (func (+ row rowOff) (+ col colOff)))
               [[-0.25 -0.25] [0.25 -0.25] [-0.25 0.25] [0.25 0.25]])))

(defn fnToCanvasAntiAliased [func width height]
  (let [antiAliasFun
        (fn [row col] (applyFunAntialiasing func row col))]
    (fnToCanvas antiAliasFun width height)
    )
  )


(defn getDataRows [canvas]
  (let [lines (mapv flatten (:data canvas))
        linesInt (mapv floatToInt lines)]
    (mapv (fn [row] (mapv (fn [val] (str val)) row)) linesInt))
  )

(defn addValToPpmLine [{lines :lines current :current} val maxChars]
  (if (>= (+ (count current) (count val)) maxChars)
    {:lines (conj lines (Strings/triml current))
     :current val}
    {:lines lines
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


(defn toPpmString [canvas]
  (str (format "P3\n%d %d\n255\n" (:width canvas) (:height canvas))
       (Strings/join "\n" (toPpmLines (getDataRows canvas) 70))
       "\n"
       ))

(defn saveAsPpm [fname canvas]
  (spit fname (toPpmString canvas)))
