(ns day#.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
)

(defn solve []
   (let [ data #p (slurp "day_#_sample_data.txt")]
      (println data)
   )
  )

(solve)
