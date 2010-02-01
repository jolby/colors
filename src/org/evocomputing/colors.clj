(ns com.evocomputing.colors
  (:require [clojure.set :as set])
  (:require [clojure.contrib.math :as math]))

(def *html4-colors-name-hex-map*
     {
      'black'    0x000000
      'silver'   0xc0c0c0
      'gray'     0x808080
      'white'    0xffffff
      'maroon'   0x800000
      'red'      0xff0000
      'purple'   0x800080
      'fuchsia'  0xff00ff
      'green'    0x008000
      'lime'     0x00ff00
      'olive'    0x808000
      'yellow'   0xffff00
      'navy'     0x000080
      'blue'     0x0000ff
      'teal'     0x008080
      'aqua'     0x00ffff
      })

(defn hex-to-rgb
  "Convert a color given in hexidecimal format into a vector of the 3
  rgb integer values"
  [hexcolor]
  (into []
        (reverse (for [n (range 0 3)]
                   (bit-and (bit-shift-right hexcolor (bit-shift-left n 3)) 0xff)))))

(def *html4-colors-name-rgb-map*
     (into {} (for [[k v] *html4-colors-name-hex-map*] [k (hex-to-rgb v)])))

(defn hue-to-rgb
  ""
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
  ""
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
  Hue, Saturation, Lightness."
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
        (println (format "r: %s g: %s b: %s min: %s max: %s delta: %s H: %s S: %s L: %s"
                         r g b min max delta h s l))
        [(mod h 360) (* 100 s) (* 100 l)]))
