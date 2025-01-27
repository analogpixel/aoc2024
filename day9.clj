(ns day9.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]]
            )
  )

(def file_in "day_9_sample_data.txt")

(defn build-disk-string [d]
  (let [
        idx (nth d 0)
        blocks (nth d 1)
        spaces (nth d 2)
        ]
    (str (reduce str (repeat blocks idx)) (reduce str (repeat spaces ".")))
    )
  )


; find the next empty space starting from position lpt
(defn find-next-space [lpt v]
  (cond 
    (> lpt (count v)) false
    (= (v lpt) ".") lpt
    :else (find-next-space (inc lpt) v)
    )
  )

(defn find-next-number [rpt v]
  (cond
    (= rpt 0) false
    (not (= (v rpt) ".")) rpt
    :else (find-next-number (dec rpt) v)
    )
  )

(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))

; left pointer points to next empty spot
; right point points to current number to move
; when left and right cross end the loop
(defn compact-string [lpt rpt v]
  (if (>= lpt rpt) v
    (let [ v (swap v lpt rpt )]
      (compact-string (find-next-space lpt v) (find-next-number rpt v) v)
      )
    )
  )

(defn checksum [s]
  (reduce +  (map-indexed (fn [i x] (if (= x ".") 0 (* i (Integer/parseInt x)))) s  ))
  )


(defn solve []
  (let [ data  (as-> file_in v
                 (slurp v) 
                 (str/trim-newline v)
                 (mapv #(Integer/parseInt %) (str/split v #""))
                 (partition 2 2 '[0] v)
                 (map-indexed (fn [i x] (conj x i)) v)
                 (mapv build-disk-string v)
                 (reduce  str v)
                 (str/split v #"")
                 )
        ]
    (print data "\n")
    (print (checksum (compact-string (find-next-space 0 data) (dec (count data)) data)))
    ; (print (swap data 
    ;              (find-next-space 0 data) 
    ;              (find-next-number (dec (count data)) data)
    ;              )
    ;        )
    )
  )

(solve)
