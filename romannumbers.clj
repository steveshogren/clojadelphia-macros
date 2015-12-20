

(defmulti mappings identity)
(defmethod mappings 1 [a] "I")
(defmethod mappings 4 [a] "IV")
(defmethod mappings 5 [a] "V")
(defmethod mappings 9 [a] "IX")
(defmethod mappings 10 [a] "X")
(defmethod mappings 40 [a] "XL")
(defmethod mappings 50 [a] "L")
(def increments [1 4 5 9 10 40 50])

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
               (str ret (mappings next)))))))

(to-roman 9)
