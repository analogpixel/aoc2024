(ns day1.core
  (:require [clojure.string :as str])
  (:gen-class))


(defn -main []
  (let [ 
        ;rows (map (fn [data] (mapv (fn [a] (Integer/parseInt a)) (str/split data #"\s+"))) (str/split (slurp "sample_data.txt") #"\n" ))
        rows (map (fn [data] (mapv #(Integer/parseInt %) (str/split data #"\s+"))) (str/split (slurp "data.txt") #"\n" ))
        row1 (sort (map first rows))
        row2 (sort (map last rows))
        difference (mapv (fn [x y] (Math/abs (- x y))) row1 row2)
        score (reduce + difference)
        f (frequencies row2)
        sim_score (reduce + (mapv (fn [x] (* x (get f x 0))) row1))
       ]

    (println score sim_score)
    )
  )

(-main)
