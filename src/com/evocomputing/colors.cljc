;; by Joel Boehland http://github.com/jolby/colors
;; February 4, 2010

;; Copyright (c) Joel Boehland, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
    ^{:doc
       "Color manipulation routines. This code was heavily infuenced by the
color.rb module in the ruby SASS project, and the colorspace packages in R.


Further references:
HSL and HSV:
http://en.wikipedia.org/wiki/Luminance-Hue-Saturation
RGB color space:
http://en.wikipedia.org/wiki/RGB_color_space
http://en.wikipedia.org/wiki/Hue#Computing_hue_from_RGB
http://www.w3.org/TR/css3-color/#hsl-color

SASS color module:
http://github.com/nex3/haml/blob/master/lib/sass/script/color.rb

R colorspace package:
http://cran.r-project.org/web/packages/colorspace/index.html

"
       :author "Joel Boehland"}

    com.evocomputing.colors
  #?(:clj (:import (java.awt Color)))
  (:require [com.evocomputing.colors.palettes.webcolors :as wc]
            [com.evocomputing.utils :as utils]
            [com.evocomputing.macros :refer [def-color-bin-op create-color-with-meta]]
            #?(:cljs [goog.string :refer [format]])
            #?(:clj [clojure.math.numeric-tower :refer [abs round]]
               :cljs [com.evocomputing.math :refer [abs round]])))

(declare rgb-int-to-components rgba-int-to-components
         rgb-int-from-components rgba-int-from-components
         rgb-to-hsl hsl-to-rgb)

(defrecord
    ^{:doc
      "Record representing a color. Default representation
    is an array of integers mapping to the respective RGB(A)
    values. This structure also supports holding an array of float
    values mapping to the respective HSL values as well"}
    color
    [
     ;;4 integer array representation of the rgba values. Rgba values
     ;;must be between 0 and 255 inclusive
     rgba
     ;;3 float array holding the HSL values for this color. The
     ;;saturation and lightness must be between 0.0 and 100.0. Hue must
     ;;be between 0.0 and 360.0
     hsl])

(def allowable-rgb-keys
  "The keyword arguments which may be used to create an RGB color."
  #{:r :red :g :green :b :blue})

(def allowable-hsl-keys
  "The keyword arguments which may be used to create an HSL color."
  #{:h :hue :s :saturation :l :lightness})

;;These are the default named colors, feel free to create your own
;;named-colors palettes
(def named-colors-name-to-rgb
  "Maps the known color names to their RGB values."
  (merge wc/html4-name-to-rgb wc/x11-name-to-rgb))

(def named-colors-rgb-to-name
  "Maps the known color RGB values to their names."
  (merge wc/html4-rgb-to-name wc/x11-rgb-to-name))

(defn hexstring-to-rgba-int
  "Convert a hexadecimal string to the corresponding array of RGBA
  integer values."
  [hexstr]
  (if-let [matches (re-find #"(^#|^0[Xx])([A-Fa-f0-9]{8}|[A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$" hexstr)]
    (utils/decode-long
      (condp =  (count (matches 2))
        3 (apply str "0xff" (map #(str % %) (matches 2)))
        6 (apply str "0xff" (matches 2))
        8 (apply str "0x" (matches 2))))
    (utils/throw-with-type :illegal-argument (str "Unrecognized color, " hexstr))))

;;Resolution/normalize code taken from Ruby color:
;;http://rubyforge.org/projects/color
(def color-epsilon
  "The maximum resolution for color math; if any value is less than or
  equal to this value, it is treated as zero."
  0.00001)

(def color-tolerance
  "The tolerance for comparing the components of two colors. In general,
  colors are considered equal if all of their components are within this
  tolerance value of each other."
  0.0001)

(defn within-tolerance?
  "Check whether the two values are close enough to be considered the same."
  [fval1 fval2]
  (<= (abs (- fval1 fval2)) color-tolerance))

(defn near-zero?
  "Returns true if the fvalue is less than color-epsilon."
  [fval]
  (<= (abs fval) color-epsilon))

(defn near-zero-or-less?
  "Returns true if the fvalue is within color-epsilon of zero or less
  than zero."
  [fval]
  (or (< fval 0.0) (near-zero? fval)))

(defn near-one?
  "Returns true if fvalue is within color-epsilon of one."
  [fval]
  (near-zero? (- 1.0 fval)))

(defn rgb-int?
  "If the passed in value is an integer in the range 0 - 255
  inclusive, return true, otherwise return false."
  [rgb-int]
  (and (integer? rgb-int) (and (>= rgb-int 0) (<= rgb-int 255))))

(defn unit-float?
  "Return true if passed in float is in the range 0.0 - 1.0, false otherwise."
  [fval]
  (and (>= fval 0.0) (<= fval 1.0)))

(defn percent-float?
  "Return true if passed in float is in the range 0.0 - 100.0, false otherwise."
  [fval]
  (and (>= fval 0.0) (<= fval 100.0)))

(defn circle-float?
  "Return true if passed in float is in the range 0.0 - 360.0, false otherwise."
  [fval]
  (and (>= fval 0.0) (<= fval 360.0)))

(defn clamp-rgb-int
  "Clamp the integer value to be within the range 0 - 255, the legal
  values for an RGB color component."
  [rgb-int]
  (max (min rgb-int 255) 0))

(defn clamp-unit-float
  "Clamp the floating point value to be within the range 0 - 1.0."
  [ufloat]
  (max (min ufloat 1.0) 0.0))

(defn clamp-percent-float
  "Clamp the floating point value to be within the range 0 - 100.0."
  [pfloat]
  (max (min pfloat 100.0) 0.0))

(defn clamp-hue
  "Clamp the hue value so that it lies in the range 0.0 - 360.0."
  [hue]
  (mod hue 360.0))

(defn unit-float-to-rgba-int
  "Check that the passed in float is in the range 0.0 - 1.0, then
convert it to the appropriate integer in the range 0 - 255."
  [fval]
  (utils/throw-if-not (unit-float? fval)
                      "fval must be a float between 0.0 and 0.1: %s" fval)
  (int (+ 0.5 (* fval 255))))

(defn rgb-int-to-unit-float
  "Convert the integer in range 0 - 255 to float in range 0.0 - 1.0."
  [rgb-int]
  (utils/throw-if-not (rgb-int? rgb-int) "Must be integer in range 0 - 255")
  (/ rgb-int 255.0))

(defn maybe-convert-alpha
  "If alpha is a float value, try to convert to integer in range 0 -
  255, otherwise return as-is."
  [alpha]
  (if (rgb-int? alpha) alpha
      (do
        (utils/throw-if-not (unit-float? alpha)
                            "alpha must be an integer in range 0 - 255 or unit float: %s" alpha)
        (unit-float-to-rgba-int alpha))))

(defn check-rgb
  "Check that every element in the passed in rgba sequence is an
  integer in the range 0 - 255."
  ([rgb]
   (utils/throw-if-not (and (= (count rgb) 3) (every? #'rgb-int? rgb))
                       "Must contain 3 integers in range 0 - 255: %s" rgb)
   rgb)
  ([r g b]
   (utils/throw-if-not (every? #'rgb-int? [r g b])
                       "Must contain 3 integers in range 0 - 255: %s" [r g b])))

(defn check-rgba
  "Check that every element in the passed in rgba sequence is an
  integer in the range 0 - 255."
  ([rgba]
   (utils/throw-if-not (and (= (count rgba) 4) (every? #'rgb-int? rgba))
                       "Must contain 4 integers in range 0 - 255: %s" rgba)
   rgba)
  ([r g b a]
   (utils/throw-if-not (every? #'rgb-int? [r g b a])
                       "Must contain 4 integers in range 0 - 255: %s" [r g b a])))

(defn check-hsl
  "Check that every element is of the format:
  - 1st, H (Hue): Float value in the range of: 0.0 - 360.0.
  - 2nd, S (Saturation): Float value in the range: 0.0 - 100.0.
  - 3rd, L (Lightness or Luminance): Float value in the range 0.0 - 100.0."
  ([hsl]
   (utils/throw-if-not (and (= (count hsl) 3) (not (some nil? hsl)))
                       "Must contain 3 floats representing HSL: %s" hsl)
   (check-hsl (hsl 0) (hsl 1) (hsl 2))
   [(clamp-hue (hsl 0)) (hsl 1) (hsl 2)])
  ([h s l] (utils/throw-if-not (and (circle-float? (clamp-hue h))
                                    (percent-float? s) (percent-float? l))
                               "Elements must be of the form:
H (Hue): Float value in the range of: 0.0 - 360.0
S (Saturation): Float value in the range: 0.0 - 100.0
L (Lightness or Luminance): Float value in the range 0.0 - 100.0
%s %s %s" h s l)))


(defn create-color-dispatch
  "Inspect the arguments and determine which version of the
  create-color multimethod to invoke."
  ([args]
   (cond
     (or (symbol? args) (string? args) (keyword? args))           ::symbolic-color
     (integer? args)                                              ::rgb-int
     (and (map? args) (some allowable-rgb-keys (keys args)))      ::rgb-map
     (and (map? args) (some allowable-hsl-keys (keys args)))      ::hsl-map
     (and (or (seq? args) (seqable? args)) (#{3 4} (count args))) ::rgb-array
     #?(:clj (= (class args) Color))                              #?(:clj Color)
     :else                                                        (utils/throw-with-type :illegal-argument
                                  (format "Don't know how to process args: %s" args))))
  ([arg & others]
   (let [args (conj others arg)]
     (cond
       (and (keyword? arg) (some allowable-rgb-keys args))          ::rgb-map
       (and (keyword? arg) (some allowable-hsl-keys args))          ::hsl-map
       (and (or (seq? args) (seqable? args)) (#{3 4} (count args))) ::rgb-array
       :else                                                        (utils/throw-with-type :illegal-argument
                                    (format "Don't know how to process args: %s" arg))))))

(defmulti create-color
  "Create a color struct using the passed in args.

This will create a color struct that has RGBA integer values in the range:
- R (Red): Integer in range 0 - 255
- G (Green): Integer in range 0 - 255
- B (Blue): Integer in range 0 - 255
- A (Alpha): Integer in range 0 - 255, with default as 255 (100% opacity)

And HSL values with the range:
- H (Hue): Float value in the range of: 0.0 - 360.0
- S (Saturation): Float value in the range: 0.0 - 100.0
- L (Lightness or Luminance): Float value in the range 0.0 - 100.0

This multimethod is very liberal in what it will accept to create a
color. Following is a list of acceptable formats:

Single Arg
- Symbolic: Either a string or keyword or symbol that matches an entry
in the symbolic color pallete. Currently, this is defaults to the
html4 colors map and x11 colors map, but the end user of this library
can set any named palette they want.

  examples:
  (create-color \"blue\")
  (create-color :blue)

- Hexstring: A hex string representation of an RGB(A) color
  examples:
  (create-color \"0xFFCCAA\")
  (create-color \"#FFCCAA\")
  (create-color \"Ox80FFFF00\") ;; alpha = 128

- Integer: An integer representation of an RGB(A) color
  examples:
  (create-color 0xFFCCAA) ;; integer in hexidecimal format
  (create-color 16764074) ;; same integer in decimal format

- Sequence or array of RGB(A) integers
  :examples
  (create-color [255 0 0])
  (create-color [255 0 0 128]) ;;alpha = 128

- Map of either RGB (A) kw/values or HSL(A) kw/values
  Allowable RGB keys: :r :red :g :green :b :blue
  Allowable HSL keys: :h :hue :s :saturation :l :lightness

  examples:
  (create-color {:r 255 :g 0 :blue 0})
  (create-color {:r 255 :g 0 :blue 0 :a 128})
  (create-color {:h 120.0 :s 100.0 :l 50.0})
  (create-color {:h 120.0 :s 100.0 :l 50.0 :a 128})

Multiple Arg
- Sequence or array of RGB(A) integers
  :examples
  (create-color 255 0 0)
  (create-color 255 0 0 128) ;;alpha = 128

- Assoc list of either RGB (A) kw/values or HSL(A) kw/values
  Allowable RGB keys: :r :red :g :green :b :blue
  Allowable HSL keys: :h :hue :s :saturation :l :lightness

  examples:
  (create-color :r 255 :g 0 :blue 0)
  (create-color :r 255 :g 0 :blue 0 :a 128)
  (create-color :h 120.0 :s 100.0 :l 50.0)
  (create-color :h 120.0 :s 100.0 :l 50.0 :a 128)
"

  create-color-dispatch)

(defmethod create-color ::symbolic-color [^java.lang.String colorsym]
  (letfn [(stringify [colorsym]
             (if (or (symbol? colorsym) (keyword? colorsym))
               (.toLowerCase (name colorsym))
               (.toLowerCase ^java.lang.String colorsym)))]
    (let [colorsym (stringify colorsym)]
      (if-let [rgb (named-colors-name-to-rgb colorsym)]
        (create-color rgb)
        (create-color
         (rgba-int-to-components (hexstring-to-rgba-int colorsym)))))))

(defmethod create-color ::rgb-int [rgb-int]
  (create-color (rgba-int-to-components rgb-int)))

(defmethod create-color ::rgb-array [rgb-array & others]
  (let [rgb-array (if others (vec (conj others rgb-array)) rgb-array)
        ;;if alpha wasn't provided, use default of 255
        alpha (if (or (= 3 (count rgb-array)) (nil? (rgb-array 3))) 255
                  (maybe-convert-alpha (rgb-array 3)))
        rgba (conj (into [] (take 3 rgb-array)) alpha) ]
    (check-rgba rgba)
    (create-color-with-meta
      (->color rgba
               (rgb-to-hsl (rgba 0) (rgba 1) (rgba 2))))))

(defmethod create-color ::rgb-map [rgb-map & others]
  (let [rgb-map (if others (apply assoc {} (vec (conj others rgb-map))) rgb-map)
        ks (keys rgb-map)
        rgb (into [] (map #(rgb-map %)
                           (map #(some % ks)
                                '(#{:r :red} #{:g :green} #{:b :blue}))))
        alpha (or (:a rgb-map) (:alpha rgb-map))
        rgba (check-rgba (if alpha (conj rgb alpha) (conj rgb 255)))]
    (create-color rgba)))

(defmethod create-color ::hsl-map [hsl-map & others]
  (let [hsl-map (if others (apply assoc {} (vec (conj others hsl-map))) hsl-map)
        ks (keys hsl-map)
        hsl (check-hsl (into [] (map #(float (hsl-map %))
                                     (map #(some % ks)
                                          '(#{:h :hue} #{:s :saturation} #{:l :lightness})))))
        rgb     (hsl-to-rgb (hsl 0) (hsl 1) (hsl 2))
        alpha   (maybe-convert-alpha (or (:a hsl-map) (:alpha hsl-map) 255))
        rgba    (check-rgba (if alpha (conj rgb alpha) (conj rgb 255)))]
    (create-color-with-meta (->color rgba hsl))))

#?(:clj (defmethod create-color Color [^java.awt.Color color]
          (create-color [(.getRed color) (.getGreen color)
                         (.getBlue color) (.getAlpha color)])))

(defn red "Return the red (int) component of this color." [color] ((:rgba color) 0))
(defn green "Return the green (int) component of this color." [color] ((:rgba color) 1))
(defn blue "Return the blue (int) component of this color." [color] ((:rgba color) 2))
(defn hue "Return the hue (float) component of this color." [color] ((:hsl color) 0))
(defn saturation "Return the saturation (float) component of this color." [color] ((:hsl color) 1))
(defn lightness "Return the lightness (float) component of this color." [color] ((:hsl color) 2))
(defn alpha "Return the alpha (int) component of this color." [color] ((:rgba color) 3))


(defn rgb-int
  "Return a integer (RGB) representation of this color."
  [color]
  (rgb-int-from-components (red color) (green color) (blue color)))

(defn rgba-int
  "Return a integer (RGBA) representation of this color."
  [color]
  (rgba-int-from-components (red color) (green color) (blue color) (alpha color)))

(defn rgba-hexstr
  "Return the hexcode string representation of this color."
  [color]
  (format "#%08X" (rgba-int color)))

(defn rgb-hexstr
  "Return the hexcode string representation of this color."
  [color]
  (format "#%06X" (rgb-int color)))

(defn color-name
  "If there is an entry for this color value in the symbolic color
  names map, return that. Otherwise, return the hexcode string of this
  color's rgba integer value."
  [color]
  (if-let [color-name (named-colors-rgb-to-name (take 3 (:rgba color)))]
    color-name
    (format "%#08x" (rgba-int color))))

#?(:clj (defn awt-color
          "Return a java.awt.Color object using this color's rgba components."
          [color]
          (Color. (rgba-int color) true)))

#?(:clj (defmethod print-method ::color [color writer]
          (print-method (format "#<color: %s R: %d, G: %d, B: %d, H: %.2f, S: %.2f, L: %.2f, A: %d>"
                                (color-name color) (red color) (green color) (blue color)
                                (hue color) (saturation color) (lightness color) (alpha color))
                        writer)))

(defn color=
  "Return true if rgba components are equal, and hsl float components
  are within tolerance."
  [color1 color2]
  (and (= (:rgba color1) (:rgba color2))
       (and (<= (abs (- (hue color1) (hue color2))) color-tolerance)
            (<= (abs (- (saturation color1) (saturation color2))) color-tolerance)
            (<= (abs (- (lightness color1) (lightness color2))) color-tolerance))))

(defn rgb-int-from-components
  "Convert a vector of the 3 rgb integer values into a color given in
  numeric rgb format."
  [r g b]
  (bit-or (bit-shift-left (bit-and r 0xFF) 16)
          (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                 (bit-shift-left (bit-and b 0xFF) 0))))

(defn rgba-int-from-components
  "Convert a vector of the 4 rgba integer values into a color given in
  numeric rgb format."
  [r g b a]
  (bit-or (bit-shift-left (bit-and a 0xFF) 24)
          (bit-or (bit-shift-left (bit-and r 0xFF) 16)
                  (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                          (bit-shift-left (bit-and b 0xFF) 0)))))

(defn rgb-int-to-components
  "Convert a color given in numeric rgb format into a vector of the 3
  rgb integer values."
  [rgb-int]
  (into []
        (reverse
          (for [n (range 0 3)]
            (bit-and
              (unsigned-bit-shift-right rgb-int (bit-shift-left n 3)) 0xff)))))

(defn rgba-int-to-components
  "Convert a color given in numeric rgb format into a vector of the 4
  rgba integer values."
  [rgba-int]
  (conj (rgb-int-to-components rgba-int)
        (unsigned-bit-shift-right rgba-int 24)))

(def-color-bin-op color-add
  "Add the RGB values of two colors together, clamping each to a maximum of 255."
  +)

(def-color-bin-op color-sub
  "Subtract the RGB values of color2 from color 1, clamping each to a
  minimum of 0."
  -)

(def-color-bin-op color-mult
  "Multiply the RGB values of two colors, clamping each to a maximum of 255"
  *)

(def-color-bin-op color-div
  "Divide the RGB values of two colors."
  /)

(defn- mix-weights
  "Calculate the weight values to use when mixing two color
  components, taking into account the opacity of each color. Takes the
  two colors, and the percentage weight to assign the first color
  compared to the second; a value of 50.0 considers both equally,
  while a value of 25.0 would count the second color three times as
  much as the first. Returns a vector containing the weight as a
  fraction from 0 to 1, and the fractions by which values in color 1
  and color 2 should be multiplied before summing in order to obtain
  the correct mix."
  [color1 color2 weight]
  (let [p (/ weight 100.0)
        w (- (* p 2) 1)
        a (- (alpha color1) (alpha color2))
        w1 (/ (+ 1
                 (if (= (* w a) -1.0) w
                     (/ (+ w a) (+ 1 (* w a)))))
              2.0)
        w2 (- 1 w1)]
    [p w1 w2]))

(defn mix
  "Produce a blend between two colors, optionally weighted by the
  given percentage. Takes the average of each of the RGB components,
  taking into account the opacity of each color. The weight specifies
  the amount of the first color that should be included in the
  returned color. The default value of 50.0 means that half the first
  color and half the second color should be used. A value of 25.0
  would count the second color three times as much as the first."
  ([color1 color2]
   (mix color1 color2 50.0))
  ([color1 color2 weight]
   (let [[p w1 w2] (mix-weights color1 color2 weight)
         rgb (vec (map #(clamp-rgb-int (int (+ (* %1 w1) (* %2 w2))))
                       (take 3 (:rgba color1)) (take 3 (:rgba color2))))
         adj-alpha (int (+ (* (alpha color1) p) (* (alpha color2) (- 1 p))))]
     (create-color (conj rgb adj-alpha)))))

(defn- hues-for-shortest-blend-path
  "Given a pair of colors, return their hue values adjusted so that
  their average value falls the shortest distance around the hue
  circle between them, by denormalizing the smallest value if needed.
  Also makes sure that hue values are ignored for colors whose
  saturation is zero."
  [color1 color2]
  (let [base-hue1 (hue color1)
        base-hue2 (hue color2)
        hue1 (if (> (saturation color1) 0.0) base-hue1 base-hue2)
        hue2 (if (> (saturation color2) 0.0) base-hue2 base-hue1)]
    (if (< (abs (- hue1 hue2)) 180.0)
      [hue1 hue2]
      (if (< hue1 hue2)
        [(+ hue1 360.0) hue2]
        [hue1 (+ hue2 360.0)]))))

(defn mix-hsl
  "Produce a blend between two colors in the HSL color space,
  optionally weighted by the given percentage. Takes the average of
  each of the HSL components (always going the shortest distance
  around the hue circle), taking into account the opacity of each
  color. The weight specifies the amount of the first color that
  should be included in the returned color. The default value of 50.0
  means that half the first color and half the second color should be
  used. A value of 25.0 would count the second color three times as
  much as the first."
  ([color1 color2]
   (mix-hsl color1 color2 50.0))
  ([color1 color2 weight]
   (let [[p w1 w2] (mix-weights color1 color2 weight)
         [hue1 hue2] (hues-for-shortest-blend-path color1 color2)
         h (clamp-hue (+ (* hue1 w1) (* hue2 w2)))
         s (clamp-percent-float (+ (* (saturation color1) w1) (* (saturation color2) w2)))
         l (clamp-percent-float (+ (* (lightness color1) w1) (* (lightness color2) w2)))
         adj-alpha (int (+ (* (alpha color1) p) (* (alpha color2) (- 1 p))))]
     (create-color :h h :s s :l l :a adj-alpha))))

(defn adjust-hue
  "Shift the hue of the color around the color wheel by the specified
  number of degrees. Will wrap around if it goes below 0.0 or above
  360.0."
  [color degrees]
  (create-color :h (clamp-hue (+ (hue color) degrees))
                :s (saturation color)
                :l (lightness color) :a (alpha color)))

(defn saturate
  "Increase the saturation of the color by the specified amount, up to
  a maximum of 100.0."
  [color percent]
  (create-color :h (hue color)
                :s (clamp-percent-float (+ (saturation color) percent))
                :l (lightness color) :a (alpha color)))

(defn desaturate
  "Decrease the saturation of the color by the specified amount, down
  to a minimum of 0.0."
  [color percent]
  (create-color :h (hue color)
                :s (clamp-percent-float (- (saturation color) percent))
                :l (lightness color) :a (alpha color)))

(defn lighten
  "Increase the lightness of the color by the specified amount, up to
  a maximum of 100.0."
  [color percent]
  (create-color :h (hue color)
                :s (saturation color)
                :l (clamp-percent-float (+ (lightness color) percent))
                :a (alpha color)))

(defn darken
  "Decrease the lightness of the color by the specified amount, down
  to a minimum of 0.0."
  [color percent]
  (create-color :h (hue color)
                :s (saturation color)
                :l (clamp-percent-float (- (lightness color) percent))
                :a (alpha color)))

(defn adjust-alpha
  "Add an amount to the alpha (transparency) of the color, keeping it
  within the allowable range of 0-255."
   [color unit-float-adj]
  (create-color :h (hue color)
                :s (saturation color)
                :l (lightness color)
                :a (clamp-unit-float (+ (rgb-int-to-unit-float (alpha color))
                                        unit-float-adj))))

(defn grayscale
  "Remove all hue from the color, converting it to a gray with the
  same lightness level."
  [color]
  (desaturate color 100.0))

(defn opposite
  "Shift the hue of the color halfway around the color weel, to the
  opposite color."
 [color]
 (adjust-hue color 180))

(defn hue-to-rgb
  "Convert hue color to rgb components
  Based on algorithm described in:
  http://en.wikipedia.org/wiki/Hue#Computing_hue_from_RGB
  and:
  http://www.w3.org/TR/css3-color/#hsl-color"
  [m1, m2, hue]
  (let* [h (cond
           (< hue 0) (inc hue)
           (> hue 1) (dec hue)
           :else hue)]
        (cond
         (< (* h 6) 1) (+ m1 (* (- m2 m1) h 6))
         (< (* h 2) 1) m2
         (< (* h 3) 2) (+ m1 (* (- m2 m1) (- (/ 2.0 3) h) 6))
         :else m1)))

(defn hsl-to-rgb
  "Given color with HSL values return vector of r, g, b.

  Based on algorithms described in:
  http://en.wikipedia.org/wiki/Luminance-Hue-Saturation#Conversion_from_HSL_to_RGB
  and:
  http://en.wikipedia.org/wiki/Hue#Computing_hue_from_RGB
  and:
  http://www.w3.org/TR/css3-color/#hsl-color"
  [hue saturation lightness]
  (let* [h (/ hue 360.0)
         s (/ saturation 100.0)
         l (/ lightness 100.0)
         m2 (if (<= l 0.5) (* l (+ s 1))
                (- (+ l s) (* l s)))
         m1 (- (* l 2) m2)]
        (into []
              (map #(round (* 0xff %))
                   [(hue-to-rgb m1 m2 (+ h (/ 1.0 3)))
                    (hue-to-rgb m1 m2 h)
                    (hue-to-rgb m1 m2 (- h (/ 1.0 3)))]))))

(defn rgb-to-hsl
    "Given the three RGB values, convert to HSL and return vector of
  Hue, Saturation, Lightness.

  Based on algorithm described in:
  http://en.wikipedia.org/wiki/Luminance-Hue-Saturation#Conversion_from_RGB_to_HSL_overview"
  [red green blue]
  (let* [r (/ red 255.0)
         g (/ green 255.0)
         b (/ blue 255.0)
         min (min r g b)
         max (max r g b)
         delta (- max min)
         l (/ (+ max min) 2.0)
         h (condp = max
                  min 0.0
                  r (* 60 (/ (- g b) delta))
                  g (+ 120 (* 60 (/ (- b r) delta)))
                  b (+ 240 (* 60 (/ (- r g) delta))))
         s (cond
            (= max min) 0.0
            (< l 0.5) (/ delta (* 2 l))
            :else (/ delta (- 2 (* 2 l))))]
        [(mod h 360.0) (clamp-percent-float (* 100.0 s)) (clamp-percent-float (* 100.0 l))]))
