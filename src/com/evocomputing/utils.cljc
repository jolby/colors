(ns com.evocomputing.utils
  #?(:cljs (:require [goog.string :refer [format]])))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn parse-float [s]
  #?(:clj (Float/parseFloat s)
     :cljs (js/parseFloat s)))

(defn decode-long [s]
  #?(:clj (Long/decode s)
     :cljs (js/parseInt s)))

(defn throw-with-type [type msg]
  (throw #?(:clj (case type
                   :exception (Exception. msg)
                   :illegal-argument (IllegalArgumentException. msg))
            :cljs (js/Error. (str type ":" msg)))))

(defn throw-if-not
  "Throws an Exception if test is false. Arguments are those
  documented for throwf."
  [test & args]
  (when-not test
    (let [message (apply format args)]
      (throw-with-type :exception message))))
