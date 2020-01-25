(defn collector [x]
  (reduce (fn [m next]
            (update m next (fn [x] (inc (or x 0)))))
          {}
          x))

(defn sorter [x]
  (reverse (sort-by val x)))

(defn roll-d10 []
  (rand-int 10))

(defn is-balanced [rolls]
  (let [counts (map val rolls)
        average (/ (reduce + counts) (count counts))]
    average)
  )
(->> (range 0 100)
     (map (fn [_] (roll-d10)))
     (collector)
     (sorter)
     (is-balanced)
    )
