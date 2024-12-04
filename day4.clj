(ns day4.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )

; all the directions we can look
(def directions [ [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] ])

; search in direction for word
(defn look-direction [data y x direction distance]
  (mapv #(get-in data [(+ y (* % (first direction))) (+ x (* % (last direction)))]) (range 0 distance))
  ) 

(defn check-word [w]
  ;(or (= w '["X" "M" "A" "S"] ) (= w '["S" "A" "M" "X"]))
  (= w '["X" "M" "A" "S"] )
  )

(defn check-all-directions [data y x]
  (let [all_words (mapv #(look-direction data y x % 4) directions) 
        matches   (mapv check-word  all_words)
        mcount    (count (filter #(= % true) matches))
        ]
    mcount
    )
  )

(defn check-all-locations [data]
  (reduce + (for [y (range 0 (count data ))
                  x (range 0 (count (data 0 )))
                  ]
              (check-all-directions data y x)
              ) )
  )

(defn solve []
  (let [ data  (as-> "day_4_sample_data.txt" v
                 (slurp v)
                 (str/split v #"\n")
                 (mapv #(str/split % #"") v)
                 )
        ]
    (println "look-direction" (look-direction data 0 4 [0 1] 4 )) 
    (println "check-all-directions" (check-all-directions data 0 4))
    (println "check-all-locations" (check-all-locations data ))
    (println data)
    )
  )

(solve)
