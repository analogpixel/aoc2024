(ns day5.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )


; given a character ch, split string on that converting numbers to ints
(defn split-on [ch s] (mapv #(Integer/parseInt %) (str/split s ch)))

; look in the vector and make sure that before_value appears before after_value
(defn check-rule [check_vector, before_value, after_value]
  (let [idx1 (.indexOf check_vector before_value)
        idx2 (.indexOf check_vector after_value)
        ]
    (if (or (= idx1 -1)  (= idx2 -1)) 
      true
      (<  (.indexOf check_vector before_value) (.indexOf check_vector after_value))
      )
  )
)

(defn get-middle [pages] 
  (int  (/ (count pages) 2))
  )

; given a list of pages and rules  check to make sure the book is correct
(defn check-book [pages rules]
  (if 
  (every? true?
          (for [pg pages
                r  rules
                ]
            (check-rule pages (first r) (second r))
            )
          )
  (pages (get-middle pages))
  0
  )
)

(defn solve []
  (let [ data (as-> "day_5_sample_data.txt" v
                (slurp v)
                (str/split v #"\n\s*\n")
                )
        rules   (mapv #(split-on #"\|" %) (str/split (data 0) #"\n"))
        books   (mapv #(split-on #"," %) (str/split (data 1) #"\n"))
        solution_1  (reduce + (mapv #(check-book % rules) books))
        ]
    (println solution_1)
    )
  )

(solve)
