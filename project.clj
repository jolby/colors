(defproject com.evocomputing/colors
  "1.0.1-SNAPSHOT"
  :description "Utilities for color manipulations.
This is mostly code ported from  the color module in SASS."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.3"]
                 [org.clojure/core.incubator "0.1.3"]]

  :autodoc { :name "colors"
            :description "Color and colorspace calculation, manipulation and conversion in Clojure."
            :page-title "Colors API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/colors/blob/"
            :web-home "http://jolby.github.com/colors/"})
