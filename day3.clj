(ns day3.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
)

; convert a vector of strings to a vector of integers
(defn to-int [v] (mapv #(Integer/parseInt %) v))

; multiply the first and last elements of a vector
(defn vmult [v] (* (first v) (last v)))

(defn solve []
   (let [ solution_1 #p ( as-> "day_3_sample_data.txt" v
                    (slurp v) ; read the data
                    (re-seq #"mul\((\d+),(\d+)\)" v)  ; strip out the digits
                    (mapv rest v)  ; remove the full match
                    (mapv to-int v) ; convert everything to integers
                    (mapv vmult v) ; multiply the first and last elements
                    (reduce + v) ; sum the results
                  )
                 ]
      (println solution_1)
   )
  )

(solve)
