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
  (= w '["X" "M" "A" "S"] )
  )

(defn check-mas [w]
  (or (= w '["M" "A" "S"]) (= w '["S" "A" "M"] ) )
  )

(defn check-x-directions [data y x]
  (let [word1   (look-direction data (+ y 1) (+ x 1) [-1 -1] 3) 
        word2   (look-direction data (+ y 1) (- x 1) [-1  1] 3) 
        match   (and (check-mas word1) (check-mas  word2))
        ]
    (if  match 1 0)
    )
  )

(defn check-all-directions [data y x]
  (let [all_words (mapv #(look-direction data y x % 4) directions) 
        matches   (mapv check-word  all_words)
        mcount    (count (filter #(= % true) matches))
        ]
    mcount
    )
  )

(defn check-all-locations [ data checkfn ]
  (reduce + (for [y (range 0 (count data ))
                  x (range 0 (count (data 0 )))
                  ]
              (checkfn data y x)
              ) )
  )

(defn solve []
  (let [ data  (as-> "day_4_data.txt" v
                 (slurp v)
                 (str/split v #"\n")
                 (mapv #(str/split % #"") v)
                 )
        ]
    (println "check-all-locations" (check-all-locations data check-all-directions))
    (println "check-x-locations"   (check-all-locations data check-x-directions))
    (println data)
    )
  )

(solve)
