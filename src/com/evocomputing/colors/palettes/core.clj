;; by Joel Boehland http://github.com/jolby/colors
;; April 16, 2010

;; Copyright (c) Joel Boehland, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns com.evocomputing.colors.palettes.core
  (:use [com.evocomputing.colors :only (create-color)]))


(defn inclusive-seq [n start end]
  "Return n evenly spaced points along the range start - end (inclusive)"
  (assert (< n 1)
  (condp = n
    1 [start]
    2 [start end]
    (conj (loop [acc [] step (/ (- end start) (- n 1)) num start idx 0]
            (if (= idx (dec n)) acc
                (recur (conj acc num) step (+ step num) (inc idx)))) end)))

(defn rainbow-hsl
  "Computes a rainbow of colors (qualitative palette) defined by
different hues given a single value of each saturation and lightness.

Arguments:
numcolors: Number of colors to be produced in this palette.

Optional Arguments:
:s (keyword) saturation. 0.0 - 100.0 (default 50.0)
:l (keyword) lightness. 0.0 - 100.0. (default 70.0)
:start (keyword) hue where rainbow starts. 0 - 360 (default 0)
:end (keyword) hue where rainbow ends. 0 - 360 (default: (* 360 (/ (- numcolors 1) numcolors)))

"
  [numcolors & opts]
  (let [opts (merge {:s 50.0 :l 70.0 :start 0 :end (* 360 (/ (- numcolors 1) numcolors))}
                    (when opts (apply assoc {} opts)))
        hvals (inclusive-seq numcolors (:start opts) (:end opts))]
    (map #(create-color :h (float %) :s (:s opts) :l (:l opts))
         hvals)))

(defn diverge-hsl
  "Compute a set of colors diverging
from a neutral center (grey or white, without color) to two
different extreme colors (blue and red by default). For the
diverging HSL colors, again two hues :h are needed, a maximal
saturation ':s' and two lightnesses ':l'.  The colors are then created by
an interpolation between the full color hsl1,
a neutral color hsl and the other full color hsl2.

Arguments:
numcolors: Number of colors to be produced in this palette.

Optional Arguments:
:h-start (keyword) starting hue (default 260)
:h-end (keyword) ending hue (default 0)
:s (keyword) saturation. 0.0 - 100.0 (default 80.0)
:l-start (keyword) starting lightness. 0.0 - 100.0. (default 30.0)
:l-end (keyword) ending lightness. 0.0 - 100.0. (default 90.0)
:power (keyword) control parameter determining how saturation and lightness should
be increased (1 = linear, 2 = quadratic, etc.) (default 1.5)
"

  [numcolors & opts]
  (let [opts (merge {:h-start 260 :h-end 0
                     :s 80.0
                     :l-start 30.0 :l-end 90.0
                     :power 1.5}
                    (when opts (apply assoc {} opts)))
        diff-l (- (:l-end opts) (:l-start opts))]
    (map #(create-color :h (if (> % 0) (:h-start opts) (:h-end opts))
                        :s (* (:s opts) (Math/pow (Math/abs %) (:power opts)))
                        :l (- (:l-end opts) (* diff-l (Math/pow (Math/abs %) (:power opts)))))
         (inclusive-seq numcolors -1.0 1.0))))


(defn sequential-hsl
  "Creates a sequential palette starting at the full color
 (h :s-start :l-start) through to a light color (h :s-end :l-end) by
interpolation.

Arguments:
numcolors: Number of colors to be produced in this palette.

Optional Arguments:
:h (keyword) starting hue (default 260)
:s-start (keyword) starting saturation. 0.0 - 100.0 (default 80.0)
:l-start (keyword) starting lightness. 0.0 - 100.0. (default 30.0)
:s-end (keyword) ending saturation. 0.0 - 100.0 (default 0.0)
:l-end (keyword) ending lightness. 0.0 - 100.0. (default 90.0)
:power (keyword) control parameter determining how saturation and lightness should
be increased (1 = linear, 2 = quadratic, etc.)
"

  [numcolors & opts]
  (let [opts (merge {:h 260
                     :s-start 80.0 :l-start 30.0
                     :s-end 0.0 :l-end 90.0 :power 1.5}
                    (when opts (apply assoc {} opts)))
        diff-s (- (:s-end opts) (:s-start opts))
        diff-l (- (:l-end opts) (:l-start opts))]
    (map #(create-color :h (:h opts)
                        :s (- (:s-end opts) (* diff-s (Math/pow % (:power opts))))
                        :l (- (:l-end opts) (* diff-l (Math/pow % (:power opts)))))
         (inclusive-seq numcolors 1.0 0.0))))

(defn heat-hsl
  " Create heat palette in HSL space. By default, it goes from a red to a yellow hue, while
simultaneously going to lighter colors (i.e., increasing
lightness) and reducing the amount of color (i.e., decreasing
saturation).

Arguments:
numcolors: Number of colors to be produced in this palette.

Optional Arguments:
:h-start (keyword) starting hue (default 260)
:h-end (keyword) ending hue (default 260)
:s-start (keyword) starting saturation. 0.0 - 100.0 (default 80.0)
:l-start (keyword) starting lightness. 0.0 - 100.0. (default 30.0)
:s-end (keyword) ending saturation. 0.0 - 100.0 (default 0.0)
:l-end (keyword) ending lightness. 0.0 - 100.0. (default 90.0)
:power-saturation (keyword) control parameter determining how saturation should increase
:power-lightness (keyword) control parameter determining how lightness should increase
be increased (1 = linear, 2 = quadratic, etc.)
"

  [numcolors & opts]
  (let [opts (merge {:h-start 0 :h-end 90
                     :s-start 100.0 :s-end 30.0
                     :l-start 50.0 :l-end 90.0
                     :power-saturation 0.20 :power-lightness 1.0}
                    (when opts (apply assoc {} opts)))
        diff-h (- (:h-end opts) (:h-start opts))
        diff-s (- (:s-end opts) (:s-start opts))
        diff-l (- (:l-end opts) (:l-start opts))]
    (map #(create-color :h (- (:h-end opts) (* (- diff-h %)))
                        :s (- (:s-end opts) (* diff-s (Math/pow % (:power-saturation opts))))
                        :l (- (:l-end opts) (* diff-l (Math/pow % (:power-lightness opts)))))
         (inclusive-seq numcolors 1.0 0.0))))

(defn terrain-hsl
  "The 'terrain_hcl' palette simply calls 'heat_hcl' with
different parameters, providing suitable terrain colors."
  [numcolors & opts]
  (heat-hsl numcolors :h-start 130 :h-end 0
            :s-start 80.0 :s-end 0.0
            :l-start 60.0 :l-end 95.0
            :power-saturation 0.10 :power-lightness 1.0))
