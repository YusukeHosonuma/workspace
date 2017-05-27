(ns myapp.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo])
  (:require [mylib.core :refer :all]))

(defn -main
  "I don't do a whole lot ... yet."
  [x y]
  (println "Hello, World!")
  (println
    (str "x = " x ", y = " y ", xC(y + 1) = "
      (count (combo/combinations (range 0 (Integer. x)) (f (Integer. y)))))))
