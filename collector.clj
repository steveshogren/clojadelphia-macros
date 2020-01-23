(defn collector [x]
  (reduce (fn [m next]
            (update m next (fn [x] (inc (or x 0)))))
          {}
          x))

(defn sorter [x]
  (reverse (sort-by val x))
  )


(sorter (collector [2 2 2 2 1 4 4 7 7]))
