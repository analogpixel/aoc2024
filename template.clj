(ns day#.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )

(def file_in "day_#_sample_data.txt")

(defn solve []
  (let [ data #p (as-> file_in v
                   (slurp v) 
                   )
        ]
    (pprint data)
    )
  )

(solve)
