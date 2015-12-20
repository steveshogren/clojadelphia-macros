

(def map-num-to-string
  {1 "I"
   4 "IV"
   5 "V"
   9 "IX"
   10 "X"
   40 "XL"
   50 "L"
   90 "XC"
   100 "C"
   400 "CD"
   500 "D"
   900 "CM"
   1000 "M"})

(def increments (keys map-num-to-string))

(defn next-letter [arabic]
  (->> increments
       (filter #(>= arabic %))
       (apply max)))

(defn to-roman [arabic]
  (loop [arabic arabic
         ret ""]
    (if (= 0 arabic)
      ret
      (let [next (next-letter arabic)]
        (recur (- arabic next)
               (str ret (get map-num-to-string next)))))))

(to-roman 29)
(to-roman 448) ; CDXLVIII 

