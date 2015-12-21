(def map-num-to-string
  {1 "I"
                                        ;4 "IV"
   5 "V"
                                        ;9 "IX"
   10 "X"
                                        ;40 "XL"
   50 "L"
                                        ;90 "XC"
   100 "C"
                                        ;400 "CD"
   500 "D"
                                        ;900 "CM"
   1000 "M"})

(def alldigits {1 {1 "I" 5 "V"}
                10 {1 "X" 5 "L"}
                100 {1 "C" 5 "D"}
                1000 {1 "M" 5 "F"}})

(defn get-digits [num]
  (map #(Integer/parseInt %)
       (filter #(not= "" %) (clojure.string/split (str num) #""))))

(defn map-digits-to-place [digits]
  (reverse (map vector (list 1 10 100 1000) (reverse digits))))


(defn next-increment [digit increments]
  (apply max (filter #(>= digit %) increments)))

(defn romanize-digit [[place digit]]
  (let [lookup (alldigits place)
        increments (keys lookup)
        next-place-ones (get (alldigits (* 10 place)) 1)
        increments (conj increments 10)
        lookup (assoc lookup 10 next-place-ones)]
    (loop [digit digit
           ret []]
      (if (= 0 digit)
        (apply str ret)
        (if (get lookup (+ 1 digit))
          (str (get lookup 1)
               (get lookup (+ 1 digit)))
          (let [next-smallest-increment (next-increment digit increments)
                next-value (get increments next-smallest-increment)]
            (clojure.core.trace/trace "trse")
            (recur (- digit next-smallest-increment)
                   (conj ret next-value))))))))

(defn to-roman [arabic]
  (loop [arabic (map-digits-to-place (get-digits arabic))
         ret ""]
    (if (= 0 (count arabic))
      ret
      (recur (rest arabic)
             (str ret (romanize-digit (first arabic)))))))

(to-roman 9) ; IX
(to-roman 29) ; XXIX
(to-roman 448) ; CDXLVIII
(to-roman 400) ; CD
