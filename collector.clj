(ns katas.katas
  (:require [clojure.test :refer :all]))

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
    0 [" "]
    1 ["_" " "]
    2 [" "]
    3 ["|" " "]
    4 ["_" " "]
    5 ["|" " "]
    6 ["|" " "]
    7 ["_" " "]
    8 ["|" " "]
    [" "]))

(defn permutations [st]
  (filter (comp not nil?)
          (mapcat (fn [x]
                    (let [options (character-at x)
                          attempts (->> options
                                        (map #(lookup (apply str (assoc (vec st) x %))))
                                        (filter (comp not nil?)))]
                      attempts))
                  (range (count st))))
  )

(defn ocr [d]
  (let [[top mid bot space] (filter (fn [x] (not= "" x))
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

(defn potter [books]
  (let [uniqueCount (count (set books))
        mult (case uniqueCount
               2 0.95
               3 0.90
               4 0.80
               5 0.75
               1.0)]
    (+ (* 8 (- (count books) uniqueCount))
       (* (* mult 8) uniqueCount))))

(deftest potter-tests
  (testing "basic counting"
    (is (= 15.2 (potter ["a" "b"])))
    (is (= 21.6 (potter ["a" "b" "c"])))
    (is (= 25.6 (potter ["a" "b" "c" "d"])))
    (is (= 30.0 (potter ["a" "b" "c" "d" "e"])))
    (is (= 38.0 (potter ["a" "a" "b" "c" "d" "e"])))
    ;; (is (= 45.2 (potter ["a" "a" "b" "b" "c" "d" "e"])))
    ))

(defn removeFirst [x col]
  (first (reduce (fn [[ret, keepGoing] next]
                   (if (and keepGoing (= next x))
                     [ret false]
                     [(conj ret next) keepGoing]))
                 [[] true]
                 col)))

(deftest removeFirstTest
  (testing ""
    (is (= [1 2 3] (removeFirst 3 [1 3 2 3])))
    (is (= [3 2 3] (removeFirst 1 [1 3 2 3])))
    (is (= [1 3 2 3] (removeFirst 9 [1 3 2 3])))
    ))

(defn vecDifference [x y]
  (reduce (fn [xRet nextY]
            (if (contains? xRet nextY)
              (removeFirst nextY xRet)
              xRet)
            ) x y)
  )

(deftest vecDifferenceTest
  (testing ""
    (is (= [1 2] (vecDifference [1 2] [3 4])))
    (is (= [1 2] (vecDifference [1 2 3 4] [3 4])))
    ))

;; (+ (* 2 0.95 8) (* 5 (* 0.75 8)))

(run-tests)

