(ns day8.core
  (:require [clojure.string :as str] 
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]])
  )

;; https://github.com/clojure/math.combinatorics/



; calculate the x,y point based on the 2 nodes
(defn anti-node-point [p1 p2]
  {
   :x (+ (:x p1) (- (:x p1) (:x p2))) 
   :y (+ (:y p1) (- (:y p1) (:y p2)))
   }
  )

; can be any point along the line so scale out
(defn anti-node-points [p1 p2]
  (mapv 
    #(sorted-map
     :x (+ (:x p1) (* % (- (:x p1) (:x p2)))) 
     :y (+ (:y p1) (* % (- (:y p1) (:y p2))))
    ) 
    (range -100 100)
    )
  )

; calculate all the antinodes for a given type
(defn calc-antinodes [d pt_fn]
  (let [ c (combo/combinations d 2 ) ]
    (flatten
      (mapv #(vector 
               (pt_fn (first %) (second %)) 
               (pt_fn (second %) (first %))
               ) c)
      )
    )
  )

; make sure the point is on the map
(defn in-range? [pt w h]
  (and (>= (:x pt) 0)
       (>= (:y pt) 0)
       (< (:x pt) w)
       (< (:y pt) h)
       )
  )


(defn score-1 [d]
  (let [w (count (first d))
        h (count d)
        data (parse-labels d)
        data_keys (keys data)
        all_nodes (flatten (mapv #(calc-antinodes (get data %) anti-node-point) data_keys))
        valid_nodes (filter #(in-range? % w h) all_nodes)
        node_count (count (distinct valid_nodes))
        ]
    node_count
    )
  )

(defn score-2 [d]
  (let [w (count (first d))
        h (count d)
        data (parse-labels d)
        data_keys (keys data)
        all_nodes (flatten (mapv #(calc-antinodes (get data %) anti-node-points) data_keys))
        valid_nodes (filter #(in-range? % w h) all_nodes)
        node_count (count (distinct valid_nodes))
        ]
    node_count
    )
  )

(defn parse-labels [d]
  (group-by :label 
            (for [x (range (count (first d)))
                  y (range (count d))
                  :let [label (get-in d [y x])]
                  :when (not= label ".")
                  ]
              {:label (get-in d [y x]) :x x :y y}
              )
            )
  )

;(def file_in "day_8_sample_data.txt")
(def file_in "day_8_data.txt")

(defn solve []
  (let [ data  (as-> file_in v
                 (slurp v) 
                 (str/split v #"\n")
                 (mapv #(str/split % #"") v)
                 )
        ]
    (pprint (score-1 data))
    (pprint (score-2 data))
    )
  )

(solve)
