(ns day#.core
  (:require [clojure.string :as str])
)

(defn lte3? [x] (<= (abs x) 3))

(defn valid? [x]
  (let [all-pos (every? pos? x) 
        all-neg (every? neg?  x)
        all-lte3 (every? lte3? x)
        ]
    (and (or all-pos all-neg) all-lte3)
  )
  )

; convert vector of string numbers to ints
(defn to-int [x] (map (fn [zz] (Integer/parseInt zz)) x))

; calculate the size between each number
(defn jump-size [x] (map (fn [y] (- (first y) (last y) )) x))

(defn solve []
  (let [data ( as-> "day_2_sample_data.txt" v
                (slurp v)
                (str/split v #"\n")
                (mapv #(str/split % #" ") v)
                (mapv to-int v)
                (mapv #(partition 2 1 %) v)
                (mapv jump-size v)
              )
        valid (count (filter identity (mapv valid? data)))
        ]

    (println data)
    (println valid)
  )
  )

(solve)
