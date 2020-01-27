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
        average (/ (reduce + counts) (count counts))
        tolerance (* average 0.1)]
    (map (fn [cnt]
           (let [in-tolerance (> (+ average tolerance) cnt (- average tolerance))]
             [in-tolerance average tolerance cnt]))
         counts)))

(->> (range 0 10000)
     (map (fn [_] (roll-d10)))
     (collector)
     (sorter)
     (is-balanced)
    )

(defn lookup [st]
  (case st
    " _ | ||_|" 0
    "     |  |" 1
    99
    )
  )


(defn partition3 [s]
  (map (partial apply str) (partition 3 s)))

(defn ocr [d]
  (let [[top mid bot space] (filter (fn [x]
                              (not= "" x))
                            (clojure.string/split d #"\n"))
        letters (map (fn [t m b]  (lookup (str t m b)))
                     (partition3 top)
                     (partition3 mid)
                     (partition3 bot))]
    letters))

(ocr "
 _    
| |  |
|_|  |
      
")
