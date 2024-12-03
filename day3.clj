(ns day3.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )

; convert a vector of strings to a vector of integers
(defn to-int [v] (mapv #(Integer/parseInt %) v))

; multiply the first and last elements of a vector
(defn vmult [v] (* (first v) (last v)))

; multiply the first and last elements of a vector of strings
(defn string-mult [v] 
  (let [v (to-int v)]
    (vmult v)
    )
  )

(defn process-state [ state]
  (loop  [state state
         toggle true
         total 0
         ]
    (if (empty? state) total
      (let [check  (first state)]
        (cond 
          (= (first check) "don't()") (recur (rest state) false total)
          (= (first check) "do()")    (recur (rest state) true  total)
          :else                       (recur (rest state) toggle (if toggle (+ total (string-mult (rest check))) total ) ) 
          )
        )
      )
    )
  )

(defn solve []
  (let [ solution_1  ( as-> "day_3_data.txt" v
                         (slurp v) ; read the data
                         (re-seq #"mul\((\d+),(\d+)\)" v)  ; strip out the digits
                         (mapv rest v)  ; remove the full match
                         (mapv string-mult v) ; multiply the first and last elements 
                         (reduce + v) ; sum the results
                         )

        solution_2  ( as-> "day_3_data.txt" v
                        (slurp v) ; read the data
                        (re-seq #"(?:don't\(\)|do\(\)|mul\((\d+),(\d+)\))" v) ; get valid commands
                        (process-state  v) ; process the commands
                        )
        ]

    (println solution_1)
    (println solution_2)
    )
  )

(solve)
