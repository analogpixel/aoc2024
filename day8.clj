(ns day8.core
  (:require [clojure.string :as str] 
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]])
  )

;; https://github.com/clojure/math.combinatorics/

(def file_in "day_8_sample_data.txt")


(defn parse-labels [d]
  (group-by :label 
            (for [x (range (count (first d)))
                  y (range (count d))
                  :let [label (get-in d [y x])]
                  :when (not= label ".")
                  ]
              {:label (get-in d [y x]) :x x :y y}
              )
            )
  )

(defn solve []
  (let [ data  (as-> file_in v
                   (slurp v) 
                   (str/split v #"\n")
                   (mapv #(str/split % #"") v)
                   (parse-labels v)
                   )
        ]
    (pprint (combo/combinations (get data "A") 2 ))
    )
  )

(solve)
