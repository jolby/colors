(defproject org.clojars.brunchboy/colors
  "1.0.2-SNAPSHOT"
  :description "Utilities for color manipulations.
This is mostly code ported from  the color module in SASS."
  :url "http://jolby.github.com/colors/"
  :license "Eclipse Public License (EPL)"
  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.incubator "0.1.3"]]

  :autodoc { :name "colors"
            :description "Color and colorspace calculation, manipulation and conversion in Clojure."
            :page-title "Colors API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/colors/blob/"
            :web-home "http://jolby.github.com/colors/"}
  :plugins [[lein-ancient "0.6.5"]]
  :min-lein-version "2.0.0")
