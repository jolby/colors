;; by Joel Boehland http://github.com/jolby/colors
;; February 4, 2010

;; Copyright (c) Joel Boehland, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns com.evocomputing.colors-test
  (:import (java.awt Color))
  (:use (clojure test))
  (:use (com.evocomputing colors))
  (:require [clojure.string :as s]))

(deftest test-hsl-to-rgb
  (is (= [255 0 0] (hsl-to-rgb 360.0,100.0,50.0)))
  (is (= [255 0 0] (hsl-to-rgb 0.0,100.0,50.0)))
  (is (= [0 255 0] (hsl-to-rgb 120.0,100.0,50.0)))
  (is (= [0 0 255] (hsl-to-rgb 240.0,100.0,50.0))))

(deftest test-rgb-to-hsl
  (is (= [0.0,100.0,50.0] (rgb-to-hsl 255 0 0)))
  (is (= [120.0,100.0,50.0] (rgb-to-hsl 0 255 0)))
  (is (= [240.0,100.0,50.0] (rgb-to-hsl 0 0 255))))

(deftest test-create-color-dispatch
  (is (= :com.evocomputing.colors/symbolic-color (create-color-dispatch "0x000000")))
  (is (= :com.evocomputing.colors/symbolic-color (create-color-dispatch "#000000")))
  (is (= :com.evocomputing.colors/rgb-int (create-color-dispatch 0x000000)))
  (is (= :com.evocomputing.colors/rgb-array (create-color-dispatch [255 0 0])))
  (is (= :com.evocomputing.colors/rgb-array (create-color-dispatch 255 0 0)))
  (is (= :com.evocomputing.colors/rgb-array (create-color-dispatch [255 0 0 255])))
  (is (= :com.evocomputing.colors/rgb-array (create-color-dispatch 255 0 0 128)))
  (is (= :com.evocomputing.colors/rgb-map (create-color-dispatch {:r 255 :g 0 :blue 0 :a 255})))
  (is (= :com.evocomputing.colors/rgb-map (create-color-dispatch :r 255 :g 0 :blue 0 :a 255)))
  (is (= :com.evocomputing.colors/hsl-map (create-color-dispatch {:h 255 :s 0 :l 0})))
  (is (= :com.evocomputing.colors/hsl-map (create-color-dispatch :h 255 :s 0 :l 0)))
  (is (= java.awt.Color (create-color-dispatch (Color. 255 0 0))))
  )

(deftest test-create-color
  (is (= :com.evocomputing.colors/color (type (create-color "#fff"))))
  (is (= :com.evocomputing.colors/color (type (create-color "0x000000"))))
  (is (= :com.evocomputing.colors/color (type (create-color "#000000"))))
  (is (= :com.evocomputing.colors/color (type (create-color 0x000000))))
  (is (= :com.evocomputing.colors/color (type (create-color [255 0 0]))))
  (is (= :com.evocomputing.colors/color (type (create-color 255 0 0))))
  (is (= :com.evocomputing.colors/color (type (create-color [255 0 0 255]))))
  (is (= :com.evocomputing.colors/color (type (create-color 255 0 0 128))))
  (is (= :com.evocomputing.colors/color (type (create-color {:r 255 :g 0 :blue 0 :a 255}))))
  (is (= :com.evocomputing.colors/color (type (create-color :r 255 :g 0 :blue 0 :a 255))))
  (is (= :com.evocomputing.colors/color (type (create-color {:h 120.0 :s 100.0 :l 50.0}))))
  (is (= :com.evocomputing.colors/color (type (create-color :h 120.0 :s 100.0 :l 50.0))))
  (is (= :com.evocomputing.colors/color (type (create-color :h 120 :s 100 :l 50))))
  (is (pos? (count (with-out-str (print (create-color :h 120 :s 100 :l 50))))))
  (is (= :com.evocomputing.colors/color (type (create-color (Color. 255 0 0)))))
  ;; test bad input checking
  (is (thrown? Exception (create-color "#badhexstring")))
  (is (thrown? Exception (create-color 355 0 0)))
  (is (thrown? Exception (create-color 255 0)))
  (is (thrown? Exception (create-color :h 120.0 :s 200.0 :l 50.0)))
  (is (thrown? Exception (create-color :h 420.0 :s 100.0 :l 500.0)))
  )

(deftest test-adjust-alpha
  (is (= 192 (alpha (adjust-alpha (create-color 0 0 0 0.50) 0.25))))
  )


;; Make sure that creating a color from RGB values leads to legal
;; saturation and lightness levels. These test values were
;; formerly causing exceptions by yielding saturation or lightness
;; values slightly greater than 100.0.
(deftest test-rgb-color-creation
  (adjust-hue (create-color :r 10 :g 255 :b 43) 40)
  (adjust-hue (create-color :r 115 :g 255 :b 218) 40)
  (adjust-hue (create-color :r 250 :g 255 :b 121) 40))

(defn hsl-rgb-test-pairs []
  (let [filestr (slurp (.getPath (.getResource (clojure.lang.RT/baseLoader) "hsl-rgb.txt")))
        chunks (s/split filestr #"\n\n")
        clean-fn (fn [lines] (filter #(not= "" %) (map #(.trim %) (s/split lines #"\n"))))]
    (partition 2 (flatten
                  (for [chunk chunks]
                    (let [[hsls rgbs] (s/split chunk #"====")]
                      (interleave (clean-fn hsls)
                                  (clean-fn rgbs))))))))

(defn test-hsl-rgb-conversions []
  (let [pairs (hsl-rgb-test-pairs)
        extract-hsl (fn [hslstr]
                      (let [hsl (re-find #"hsl\((-?[0-9]+), ([0-9]+)%, ([0-9]+)%\)" hslstr)]
                        (create-color :h (Float/parseFloat (hsl 1))
                                      :s (Float/parseFloat (hsl 2))
                                      :l (Float/parseFloat (hsl 3)))))
        extract-rgb (fn [rgbstr]
                      (create-color
                       (vec (map #(Integer/parseInt %)
                                 (drop 1
                                       (re-find #"rgb\(([0-9]+), ([0-9]+), ([0-9]+)\)" rgbstr))))))]
    (for [pair pairs]
        (let [hsl-color (extract-hsl (first pair))
              rgb-color (extract-rgb (second pair))
              white (= (lightness hsl-color) 100.0)
              black (= (lightness hsl-color) 0.0)
              grayscale (or white black (= (saturation hsl-color) 0.0))]
          (throw-if-not (or grayscale
                            (within-tolerance? (hue hsl-color) (hue rgb-color)))
                        "Hues should be equal")
          (throw-if-not (or white black
                            (within-tolerance? (saturation hsl-color) (saturation rgb-color)))
                        "Saturations should be equal")
          (throw-if-not (within-tolerance? (lightness hsl-color) (lightness rgb-color))
                        "Lightnesses should be equal")))))
