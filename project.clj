(defproject com.evocomputing/colors
  "1.0.0-SNAPSHOT"
  :description "Utilities for color manipulations.
This is mostly code ported from  the color module in SASS."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]]
  :dev-dependencies [[leiningen/lein-swank "1.0.0-SNAPSHOT"]
                     [autodoc "0.7.0"]]

  :autodoc { :name "colors"
            :description "Color and colorspace calculation, manipulation and conversion in Clojure."
            :page-title "Colors API documentation"
            :copyright "Eclipse Public License (EPL)"
            :web-src-dir "http://github.com/jolby/colors/blob/"
            :web-home "http://jolby.github.com/colors/"})