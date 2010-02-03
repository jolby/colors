(ns
    #^{:doc
       "Color manipulation routines. This is mostly a port of the
color.rb module in the ruby SASS project:
http://github.com/nex3/haml/blob/master/lib/sass/script/color.rb

Further references:
HSL and HSV: http://en.wikipedia.org/wiki/Luminance-Hue-Saturation
RGB color space: http://en.wikipedia.org/wiki/RGB_color_space
http://en.wikipedia.org/wiki/Hue#Computing_hue_from_RGB
http://www.w3.org/TR/css3-color/#hsl-color
"
       :author "Joel Boehland"}

  com.evocomputing.colors
  (import (java.awt Color))
  (:use (clojure.contrib core math)))

(defstruct
    #^{:doc
       "Structure representing a color. Default representation
    is an array of integers mapping to the respective RGB(A)
    values. This structure also supports holding an array of float
    values mapping to the respective HSL values as well"}
  color
  ;;4 integer array representation of the rgba values. Rgba values
  ;;must be between 0 and 255 inclusive
  :rgb
  ;;3 float array holding the HSL values for this color. The
  ;;saturation and lightness must be between 0.0 and 100.0. Hue must
  ;;be between 0.0 and 360.0
  :hsl)

(def allowable-rgba-keys
     #{:r :red :g :green :b :blue :a :alpha})

(def allowable-hsl-keys
     #{:h :hue :s :saturation :l :lightness :a :alpha})

(defn create-color-dispatch
  ""
  [args]
  (cond
   (or (symbol? args) (string? args) (keyword? args)) ::symbolic-color
   (integer? args) ::rgb-int
   (and (map? args) (some allowable-rgba-keys (keys args))) ::rgb-map
   (and (map? args) (some allowable-hsl-keys (keys args))) ::hsl-map
   (and (or (seq? args) (seqable? args)) (= (count args) 3)) ::rgb
   (and (or (seq? args) (seqable? args)) (= (count args) 4)) ::rgba
   (= (class args) Color) Color
   true (throw (IllegalArgumentException.
               (format "Don't know how to process args: %s" args)))))

(defmacro create-color-with-meta
  "Create color with type meta"
  [& body]
  `(with-meta
     ~@body
      {:type ::color}))

(defmulti create-color
  "Create a color object using the passed in args"
  create-color-dispatch)

(defn stringify
  "Convert symbol or keyword to lowercase string name, otherwise return untouched."
  [colorsym]
  (if (or (symbol? colorsym) (keyword? colorsym))
    (.toLowerCase (name colorsym))
    colorsym))

(defmethod create-color ::symbolic-color [colorsym]
  (let [colorsym (stringify colorsym)]
    (if-let [rgb-int (html4-colors-name-to-rgbnum colorsym)]
      (create-color (rgb-int-to-components rgb-int))
      (create-color
       (rgba-int-to-components (Integer/decode colorsym))))))

(defmethod create-color ::rgb-int [rgb-int]
  (create-color (rgba-int-to-components rgb-int)))

(defmethod create-color ::rgb [rgb]
  (create-color (conj rgb 255)))

(defmethod create-color ::rgba [rgba]
  (create-color-with-meta
    (struct color rgba
            (rgb-to-hsl (rgba 0) (rgba 1) (rgba 2)))))

(defmethod create-color ::rgb-map [rgb-map]
  (let [ks (keys rgb-map)
        rgba (into [] (map #(rgb-map %)
                           (map #(some % ks)
                                '(#{:r :red} #{:g :green} #{:b :blue} #{:a :alpha}))))]
    (create-color rgba)))

(defmethod create-color ::hsl-map [hsl-map])

(defmethod create-color Color [color])

(defn red [color] ((:rgb color) 0))
(defn green [color] ((:rgb color) 1))
(defn blue [color] ((:rgb color) 2))
(defn hue [color] ((:hsl color) 0))
(defn saturation [color] ((:hsl color) 1))
(defn lightness [color] ((:hsl color) 2))
(defn alpha [color] ((:rgb color) 3))

(defn rgba-int
  ""
  [color]
  (rgba-int-from-components (red color) (green color) (blue color) (alpha color)))

(defmethod print-method ::color [color writer]
  (print-method (format "#<color: %#08x R: %d, G: %d, B: %d, A: %s>"
                        (rgba-int color) (red color) (green color) (blue color) (alpha color))
                writer))

(defn rgb-int-from-components
  "Convert a vector of the 3 rgb integer values into a color given in
  numeric rgb format"
  [r g b]
  (bit-or (bit-shift-left (bit-and r 0xFF) 16)
          (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                 (bit-shift-left (bit-and b 0xFF) 0))))

(defn rgba-int-from-components
  "Convert a vector of the 4 rgba integer values into a color given in
  numeric rgb format "
  [r g b a]
  (bit-or (bit-shift-left (bit-and a 0xFF) 24)
          (bit-or (bit-shift-left (bit-and r 0xFF) 16)
                  (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                          (bit-shift-left (bit-and b 0xFF) 0)))))

(defn rgb-int-to-components
  "Convert a color given in numeric rgb format into a vector of the 3
  rgb integer values"
  [rgb-int]
  (into []
        (reverse (for [n (range 0 3)]
                   (bit-and (bit-shift-right rgb-int (bit-shift-left n 3)) 0xff)))))

(defn rgba-int-to-components
  "Convert a color given in numeric rgb format into a vector of the 4
  rgba integer values"
  [rgba-int]
  (conj (rgb-int-to-components rgba-int)
        (bit-shift-right rgba-int 24)))

  (comment (into []
        (reverse (for [n (range 0 3)]
                   (bit-and (bit-shift-right rgb-int (bit-shift-left n 3)) 0xff)))))

(def html4-colors-name-to-rgbnum
     {
      "black"    0x000000
      "silver"   0xc0c0c0
      "gray"     0x808080
      "white"    0xffffff
      "maroon"   0x800000
      "red"      0xff0000
      "purple"   0x800080
      "fuchsia"  0xff00ff
      "green"    0x008000
      "lime"     0x00ff00
      "olive"    0x808000
      "yellow"   0xffff00
      "navy"     0x000080
      "blue"     0x0000ff
      "teal"     0x008080
      "aqua"     0x00ffff
      })

(def html4-colors-rgbnum-to-name
     (into {} (map (fn [[k v]] [v k]) html4-colors-name-to-rgbnum)))

(def html4-colors-name-to-rgb
     (into {} (for [[k v] html4-colors-name-to-rgbnum] [k (rgb-int-to-components v)])))

(def html4-colors-rgb-to-name
     (into {} (map (fn [[k v]] [v k]) html4-colors-name-to-rgb)))


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

Based on algorithm described in:
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
              (map #(math/round (* 0xff %))
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
                  min 0
                  r (* 60 (/ (- g b) delta))
                  g (+ 120 (* 60 (/ (- b r) delta)))
                  b (+ 240 (* 60 (/ (- r g) delta))))
         s (cond
            (= max min) 0
            (< l 0.5) (/ delta (* 2 l))
            :else (/ delta (- 2 (* 2 l))))]
        ;;(println (format "r: %s g: %s b: %s min: %s max: %s delta: %s H: %s S: %s L: %s"
        ;;                 r g b min max delta h s l))
        [(mod h 360) (* 100 s) (* 100 l)]))
