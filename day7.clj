(ns day7.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )


(defn list-to-int [s]
  (let [ n (str/split s #"\s+") ]
    (mapv #(bigint %) n)
    )
  )

(defn placement [total numbers]
  (letfn [ (can-total? [total current num_list] 
             (cond
               (> current total) false
               ( and (= total current) (empty? num_list)) true
               (empty? num_list) false
               :else (or (can-total? total (+ current (first num_list)) (rest num_list))
                         (can-total? total (* current (first num_list)) (rest num_list))
                         )
               )
             )
          ]

    (can-total? total (first numbers) (rest numbers))
    )
  )

(defn placement-2 [total numbers]
  (letfn [ (can-total? [total current num_list] 
             (cond
               (> current total) false
               ( and (= total current) (empty? num_list)) true
               (empty? num_list) false
               :else (or (can-total? total (+ current (first num_list)) (rest num_list))
                         (can-total? total (* current (first num_list)) (rest num_list))
                         (can-total? total (bigint (str current (first num_list))) (rest num_list))
                         )
               )
             )
          ]

    (can-total? total (first numbers) (rest numbers))
    )
  )

(defn good-count [d run_fn ]
  (reduce +
          (mapv #(if (run_fn (first %) (second %)) (first %) 0) d)
          )
  )

(def file_in "day_7_sample_data.txt")
(def file_in "day_7_data.txt")

(defn solve []
  (let [ data (as-> file_in v
                (slurp v) 
                (str/split v #"\n")
                (mapv #(str/split % #": ") v)
                (mapv #(vector (bigint (first %)) (list-to-int (last %))) v) 
                )
        solution_1 (good-count data placement)
        solution_2 (good-count data placement-2)
        ]
    (println solution_1)
    (println solution_2)
    )
  )

(solve)
