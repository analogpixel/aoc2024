(ns day6.core
  (:require [clojure.string :as str] 
            [clojure.pprint :refer [pprint]])
  )

; find the guard location
(defn find-guard [data] 
  (first (filter #(> (last %) 0) (mapv  #(vector %2 (.indexOf %1 "^") ) data (range (count data)))))
  )

; make sure the location is in bounds
(defn in-bounds [loc w h ] (and 
                             (>= (first loc) 0) 
                             (>= (last loc) 0)
                             (<= (first loc) h)
                             (<= (last loc) w )
                             )
  )

; update the map data
(defn update-map [map_data y x ] (assoc-in map_data [y x] "X"))

; rotate the direction 90 degrees (yx)
(defn rotate-position-90 [direction]
  (cond 
    (= direction [-1 0 ]) [0 1]
    (= direction [0  1]) [1 0]
    (= direction [1 0]) [0 -1]
    (= direction [0 -1]) [-1 0]
    )
  )

; count all the X's in the map data
(defn count-positions [map_data]
  (count (filter #(= % "X") (flatten map_data)))
  )

; move the guard around the map until they leave it
(defn move-guard [gloc map_data direction]
  ;(pprint map_data )
  ;(pprint "\n")

  (def w (dec (count (first map_data))))
  (def h (dec (count map_data)))

  (loop [gloc gloc 
         direction direction
         map_data map_data
         overflow 0
         ]

    (if (> overflow 8000) 
      ;(pprint map_data) ;"-1"
      -1

      (if (in-bounds gloc w h)
        (let [x (last gloc)
              y (first gloc)
              next_x (+ x (last direction))
              next_y (+ y (first direction))
              next_pos (get-in map_data [next_y next_x])
              map_data (update-map map_data y x)
              ]
          (if (= next_pos "#") 
            (recur gloc (rotate-position-90 direction) map_data (inc overflow))
            (recur [next_y next_x] direction map_data (inc overflow))
            )
          )
        map_data ; return map data when guard is out of bounds
        )
      )
    )
  )

; for each . in the map data, test if it loops
(defn for-each-dot [gloc data]
  (count
    (filter #(= % -1)
            (for [y (range (count data))
                  x (range (count (first data)))
                  ]
              (if (= (get-in data [y x]) ".")
                (move-guard gloc (assoc-in data [y x] "#") [-1 0] )
                true
                )
              )
            )
    )
  )

(defn solve []
  (let [ data  (as-> "day_6_data.txt" v
                 (slurp v)
                 (str/split v #"\n")
                 (mapv #(str/split % #"" ) v)
                 )
        gloc     (find-guard data)
        direction  [-1 0 ]  ; xy up
        ;solution_1 (move-guard gloc data direction)
        solution_2 (for-each-dot gloc data)
        ]

    (println (count-positions solution_1))
    (println solution_2)
    )
  )

(solve)
