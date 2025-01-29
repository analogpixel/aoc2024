(ns day9.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]]
            )
  )


;(def file_in "day_9_sample_data.txt")
(def file_in "day_9_data.txt")


(defn build-disk-vector [d]
  (let [
        idx (nth d 0)
        blocks (nth d 1)
        spaces (nth d 2)
        ]
    (concat (repeat blocks idx) (repeat spaces "."))
    )
  )


(defn find-next-space [lpt v]
  (loop [lpt lpt
         v v  ]
    (cond
      (> lpt (count v)) #p false
      (= (v lpt) ".") lpt
      :else (recur (inc lpt) v)
      )
    )
  )

(defn find-next-number [rpt v]
  (loop [rpt rpt
         v v  ]
    (cond
      (= rpt 0) #p false
      (not (= (v rpt) ".")) rpt
      :else (recur (dec rpt) v)
      )
    )
  )

(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))

(defn compact-string [lpt rpt v]
  (loop [lpt lpt
         rpt rpt
         v v]
    (if (> lpt rpt) v
      (let [ v (swap v lpt rpt )]
        (recur (find-next-space lpt v) (find-next-number rpt v) v)
        )
      )
    )
  )

(defn checksum [s]
  (reduce +  (map-indexed (fn [i x] (if (= x ".") 0 (* i x)))  s  ))
  )


(defn solve []
  (let [ data  (as-> file_in v
                 (slurp v) 
                 (str/trim-newline v)
                 (mapv #(Integer/parseInt %) (str/split v #""))
                 (partition 2 2 '[0] v)
                 (map-indexed (fn [i x] (conj x i)) v)
                 (vec (flatten (mapv build-disk-vector v)))
                 )
        ]
    ; (print data "\n")
    (print (checksum (compact-string (find-next-space 0 data) (find-next-number (dec (count data)) data) data)))
    )
  )

(solve)
