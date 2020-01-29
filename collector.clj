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

(comment (->> (range 0 10000)
             (map (fn [_] (roll-d10)))
             (collector)
             (sorter)
             (is-balanced)
             ))

(defn lookup [st]
  (case st
    " _ | ||_|" 0
    "     |  |" 1
    " _  _||_ " 2
    " _  _| _|" 3
    "   |_|  |" 4
    " _ |_  _|" 5
    " _ |_ |_|" 6
    " _   |  |" 7
    " _ |_||_|" 8
    " _ |_|  |" 9
    nil
    )
  )

(defn partition3 [s]
  (map (partial apply str) (partition 3 s)))

(defn character-at [idx]
  (case idx
    0 " "
    1 "_"
    2 " "
    3 "|"
    4 "_"
    5 "|"
    6 "|"
    7 "_"
    8 "|"
    " "))

(defn permutations [st]
  (filter (comp not nil?)
          (map (fn [x]
                 (let [attempt (apply str (assoc (vec st) x (character-at x)))]
                   (println (str "message" attempt) )
                   (lookup attempt)))
               (range (count st))))
  )

(defn ocr [d]
  (let [[top mid bot space] (filter (fn [x]
                              (not= "" x))
                            (clojure.string/split d #"\n"))
        letters (map (fn [t m b]
                       (let [s (str t m b)
                             value (lookup s)]
                         (if (nil? value)
                            (permutations s)
                            value
                           )))

                     (partition3 top)
                     (partition3 mid)
                     (partition3 bot))]
    letters))

(ocr "
 _     _  _     _  _  _  _  _     _  _ 
| |  | _| _||_||_ |_   ||_||_|| || | _|
|_|  ||_  _|  | _||_|  ||_|  ||_| _|  |
")
