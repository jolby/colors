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
  #?(:clj (:import (java.awt Color)))
  (:require [clojure.string :as s]
            [clojure.test :refer (is deftest)]
            [com.evocomputing.utils :as utils]
            [com.evocomputing.colors :as sut]))

(deftest test-hsl-to-rgb
  (is (= [255 0 0] (sut/hsl-to-rgb 360.0,100.0,50.0)))
  (is (= [255 0 0] (sut/hsl-to-rgb 0.0,100.0,50.0)))
  (is (= [0 255 0] (sut/hsl-to-rgb 120.0,100.0,50.0)))
  (is (= [0 0 255] (sut/hsl-to-rgb 240.0,100.0,50.0))))

(deftest test-rgb-to-hsl
  (is (= [0.0,100.0,50.0] (sut/rgb-to-hsl 255 0 0)))
  (is (= [120.0,100.0,50.0] (sut/rgb-to-hsl 0 255 0)))
  (is (= [240.0,100.0,50.0] (sut/rgb-to-hsl 0 0 255))))

(deftest test-create-color-dispatch
  (is (= :com.evocomputing.colors/symbolic-color (sut/create-color-dispatch "0x000000")))
  (is (= :com.evocomputing.colors/symbolic-color (sut/create-color-dispatch "#000000")))
  (is (= :com.evocomputing.colors/rgb-int (sut/create-color-dispatch 0x000000)))
  (is (= :com.evocomputing.colors/rgb-array (sut/create-color-dispatch [255 0 0])))
  (is (= :com.evocomputing.colors/rgb-array (sut/create-color-dispatch 255 0 0)))
  (is (= :com.evocomputing.colors/rgb-array (sut/create-color-dispatch [255 0 0 255])))
  (is (= :com.evocomputing.colors/rgb-array (sut/create-color-dispatch 255 0 0 128)))
  (is (= :com.evocomputing.colors/rgb-map (sut/create-color-dispatch {:r 255 :g 0 :blue 0 :a 255})))
  (is (= :com.evocomputing.colors/rgb-map (sut/create-color-dispatch :r 255 :g 0 :blue 0 :a 255)))
  (is (= :com.evocomputing.colors/hsl-map (sut/create-color-dispatch {:h 255 :s 0 :l 0})))
  (is (= :com.evocomputing.colors/hsl-map (sut/create-color-dispatch :h 255 :s 0 :l 0)))
  #?(:clj (is (= java.awt.Color (sut/create-color-dispatch (Color. 255 0 0)))))
  )

(deftest test-create-color
  (is (instance? com.evocomputing.colors.color (sut/create-color "#fff")))
  (is (instance? com.evocomputing.colors.color (sut/create-color "0x000000")))
  (is (instance? com.evocomputing.colors.color (sut/create-color "#000000")))
  (is (instance? com.evocomputing.colors.color (sut/create-color 0x000000)))
  (is (instance? com.evocomputing.colors.color (sut/create-color [255 0 0])))
  (is (instance? com.evocomputing.colors.color (sut/create-color 255 0 0)))
  (is (instance? com.evocomputing.colors.color (sut/create-color [255 0 0 255])))
  (is (instance? com.evocomputing.colors.color (sut/create-color 255 0 0 128)))
  (is (instance? com.evocomputing.colors.color (sut/create-color {:r 255 :g 0 :blue 0 :a 255})))
  (is (instance? com.evocomputing.colors.color (sut/create-color :r 255 :g 0 :blue 0 :a 255)))
  (is (instance? com.evocomputing.colors.color (sut/create-color {:h 120.0 :s 100.0 :l 50.0})))
  (is (instance? com.evocomputing.colors.color (sut/create-color :h 120.0 :s 100.0 :l 50.0)))
  (is (instance? com.evocomputing.colors.color (sut/create-color :h 120 :s 100 :l 50)))
  (is (pos? (count (with-out-str (print (sut/create-color :h 120 :s 100 :l 50))))))
  #?(:clj (is (instance? com.evocomputing.colors.color (sut/create-color (Color. 255 0 0)))))
  ;; test bad input checking
  (is (thrown? #?(:clj Exception, :cljs js/Error) (sut/create-color "#badhexstring")))
  (is (thrown? #?(:clj Exception, :cljs js/Error) (sut/create-color 355 0 0)))
  (is (thrown? #?(:clj Exception, :cljs js/Error) (sut/create-color 255 0)))
  (is (thrown? #?(:clj Exception, :cljs js/Error) (sut/create-color :h 120.0 :s 200.0 :l 50.0)))
  (is (thrown? #?(:clj Exception, :cljs js/Error) (sut/create-color :h 420.0 :s 100.0 :l 500.0)))
  )

(deftest test-adjust-alpha
  (is (= 192 (sut/alpha (sut/adjust-alpha (sut/create-color 0 0 0 0.50) 0.25))))
  )


;; Make sure that creating a color from RGB values leads to legal
;; saturation and lightness levels. These test values were
;; formerly causing exceptions by yielding saturation or lightness
;; values slightly greater than 100.0.
(deftest test-rgb-color-creation
  (sut/adjust-hue (sut/create-color :r 10 :g 255 :b 43) 40)
  (sut/adjust-hue (sut/create-color :r 115 :g 255 :b 218) 40)
  (sut/adjust-hue (sut/create-color :r 250 :g 255 :b 121) 40))

(deftest test-rgba-int-to-components
  (is (= (sut/rgba-int-to-components (sut/hexstring-to-rgba-int "#fff")) [255 255 255 255])))

#?(:clj (defn hsl-rgb-test-pairs []
          (let [filestr  (slurp (.getPath (.getResource (clojure.lang.RT/baseLoader) "hsl-rgb.txt")))
                chunks   (s/split filestr #"\n\n")
                clean-fn (fn [lines] (filter #(not= "" %) (map #(.trim %) (s/split lines #"\n"))))]
            (partition 2 (flatten
                           (for [chunk chunks]
                             (let [[hsls rgbs] (s/split chunk #"====")]
                               (interleave (clean-fn hsls)
                                           (clean-fn rgbs)))))))))

#?(:clj (defn test-hsl-rgb-conversions []
          (let [pairs       (hsl-rgb-test-pairs)
                extract-hsl (fn [hslstr]
                              (let [hsl (re-find #"hsl\((-?[0-9]+), ([0-9]+)%, ([0-9]+)%\)" hslstr)]
                                (sut/create-color :h (utils/parse-float (hsl 1))
                                                  :s (utils/parse-float (hsl 2))
                                                  :l (utils/parse-float (hsl 3)))))
                extract-rgb (fn [rgbstr]
                              (sut/create-color
                                (vec (map #(utils/parse-int %)
                                          (drop 1
                                                (re-find #"rgb\(([0-9]+), ([0-9]+), ([0-9]+)\)" rgbstr))))))]
            (for [pair pairs]
              (let [hsl-color (extract-hsl (first pair))
                    rgb-color (extract-rgb (second pair))
                    white     (= (sut/lightness hsl-color) 100.0)
                    black     (= (sut/lightness hsl-color) 0.0)
                    grayscale (or white black (= (sut/saturation hsl-color) 0.0))]
                (utils/throw-if-not (or grayscale
                                        (sut/within-tolerance? (sut/hue hsl-color) (sut/hue rgb-color)))
                                    "Hues should be equal")
                (utils/throw-if-not (or white black
                                        (sut/within-tolerance? (sut/saturation hsl-color) (sut/saturation rgb-color)))
                                    "Saturations should be equal")
                (utils/throw-if-not (sut/within-tolerance? (sut/lightness hsl-color) (sut/lightness rgb-color))
                                    "Lightnesses should be equal"))))))
