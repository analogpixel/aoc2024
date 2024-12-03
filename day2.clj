(ns day2.core
  (:require [clojure.string :as str])
  )

; convert vector of string numbers to ints
(defn to-int [x] (mapv (fn [zz] (Integer/parseInt zz)) x))

; calculate the size between each number
(defn jump-size [x] ( mapv (fn [y] (- (first y) (last y) )) x))

; less than or equal to 3
(defn lte3? [x] (<= (abs x) 3))

; check is the solution is valid
(defn valid? [x]
  (let [parts    (partition 2 1 x)
        jmp_size (jump-size parts)
        all-pos  (every? pos? jmp_size) 
        all-neg  (every? neg?  jmp_size)
        all-lte3 (every? lte3? jmp_size)
        ]
    (and (or all-pos all-neg) all-lte3)
    )
  )

; get all possible vectors by removing one element at a time
(defn get-all [v]
  (map #(vec (concat (subvec v 0 %)
                     (subvec v (inc %) (count v))))
       (range (count v)))
  )

; given a vector, check if removing any of the entries will make valid? true
(defn try-all [x] 
  (let [all (get-all x)]
    (some true? (map valid? all))
    ) 
  )

(defn solve []
  (let [data ( as-> "day_2_data.txt" v
               (slurp v)
               (str/split v #"\n")
               (mapv #(str/split % #"\s+") v)
               (mapv to-int v)
               )
        part_1 (count (filter identity (mapv valid? data)))
        part_2 (count (filter identity (mapv try-all data)))
        ]


    (println data)
    (println part_1)
    (println part_2)
    )
  )

(solve)
