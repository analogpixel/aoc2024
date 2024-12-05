(ns day5.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )

; swap two indexes in a vector
(defn swap-values [v idx1 idx2]
  (let [val1 (nth v idx1)
        val2 (nth v idx2)]
    (-> v
        (assoc idx1 val2)
        (assoc idx2 val1))))

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

; get middle page
(defn get-middle [pages] (int  (/ (count pages) 2)))

; given a list of pages and rules  check to make sure the book is correct
(defn check-book [pages rules]
  (for [pg pages
        r  rules
        ]
    (check-rule pages (first r) (second r))
    )
  )

; check a set of pages against all the rules
; if any rules are broken, swap the numbers and keep going
(defn check-and-fix-book [pages rules]
  (loop [r rules
         ridx 0
         pg pages
         fixed false
         ]
    (if (= ridx (count r))
      [ pg fixed]   ; no more values? return results
      (let [current_rule (r ridx)
            r1 (first current_rule)
            r2 (last current_rule)
            ]
        (if (check-rule pg r1 r2)
          (recur  r (+ 1 ridx) pg fixed) ; if the rule is ok, loop increase index
          ; else
          ; fix the error and then reset the ridx so we can re-run all the checks again 
          ; set the fixed flag for the final solution
          (recur  r 0 (swap-values pg (.indexOf pg r1) (.indexOf pg r2)) true ) 
          )
        )
      )
    )
  )

; for each fixed book return the middle page
(defn score-book-2 [pages rules]
  (let [check_book (check-and-fix-book pages rules)
        fixed      (check_book 1)
        fixed_book (check_book 0)
        ]
    (if fixed
      (fixed_book (get-middle fixed_book))
      0
      )
    )
  )

; find all valid books and return the middle page value
(defn score-book [pages rules]
  (if (every? true? (check-book pages rules))
    (pages (get-middle pages))
    0
    )
  )

;(def in_file "day_5_sample_data.txt")
(def in_file "day_5_data.txt")

(defn solve []
  (let [ data (as-> in_file v
                (slurp v)
                (str/split v #"\n\s*\n")
                )
        rules       (mapv #(split-on #"\|" %) (str/split (data 0) #"\n"))
        books       (mapv #(split-on #"," %) (str/split (data 1) #"\n"))
        solution_1  (reduce + (mapv #(score-book % rules) books))
        solution_2  (reduce + (mapv #(score-book-2 % rules) books))

        ]
    (println solution_2)
    )
  )

(solve)
