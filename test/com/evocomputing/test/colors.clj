;; by Joel Boehland http://github.com/jolby/colors
;; February 4, 2010

;; Copyright (c) Joel Boehland, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


(ns com.evocomputing.test.colors
  (import (java.awt Color))
  (:use (clojure test))
  (:use (com.evocomputing colors)))

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
  (is (= :com.evocomputing.colors/color (type (create-color "0x000000"))))
  (is (= :com.evocomputing.colors/color (type (create-color "#000000"))))
  (is (= :com.evocomputing.colors/color (type (create-color 0x000000))))
  (is (= :com.evocomputing.colors/color (type (create-color [255 0 0]))))
  (is (= :com.evocomputing.colors/color (type (create-color 255 0 0))))
  (is (= :com.evocomputing.colors/color (type (create-color [255 0 0 255]))))
  (is (= :com.evocomputing.colors/color (type (create-color 255 0 0 128))))
  (is (= :com.evocomputing.colors/color (type (create-color {:r 255 :g 0 :blue 0 :a 255}))))
  (is (= :com.evocomputing.colors/color (type (create-color :r 255 :g 0 :blue 0 :a 255))))
  (is (= :com.evocomputing.colors/color (type (create-color {:h 255 :s 0 :l 0}))))
  (is (= :com.evocomputing.colors/color (type (create-color :h 255 :s 0 :l 0))))
  (is (= :com.evocomputing.colors/color (type (create-color (Color. 255 0 0)))))
  )
