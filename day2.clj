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

(defn solve []
  (let [data ( as-> "day_2_sample_data.txt" v
                (slurp v)
                (str/split v #"\n")
                (mapv #(str/split % #" ") v)
                (mapv (fn [x] (map (fn [zz] (Integer/parseInt zz)) x)) v)
                (mapv #(partition 2 1 %) v)
                (mapv (fn [x] (mapv (fn [y] (- (first y) (last y) )) x)) v)
              )
        valid (count (filter identity (mapv valid? data)))
        ]

    (println data)
    (println valid)
  )
  )

(solve)
