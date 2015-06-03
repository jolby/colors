(defproject com.evocomputing/colors
  "1.0.1-SNAPSHOT"
  :description "Utilities for color manipulations.
This is mostly code ported from  the color module in SASS."
  :dependencies [[org.clojure/clojure "1.7.0-beta3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                                  [org.clojure/core.incubator "0.1.3"]]

  :autodoc { :name "colors"
            :description "Color and colorspace calculation, manipulation and conversion in Clojure."
            :page-title "Colors API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/colors/blob/"
            :web-home "http://jolby.github.com/colors/"}
  :plugins [[lein-ancient "0.6.5"]
            [codox "0.8.12"]]
  :codox {:src-dir-uri "http://github.com/brunchboy/colors/blob/master/"
          :src-linenum-anchor-prefix "L"
          :output-dir "target/doc"}
  :min-lein-version "2.0.0")
