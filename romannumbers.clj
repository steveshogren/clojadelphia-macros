

(defmulti twos identity)
(defmethod twos 1 [a] "I")
(defmethod twos 5 [a] "V")
(defmethod twos 10 [a] "X")
(def increments [1 5 10])

(defn to-roman [arabic]
  (->> increments
       (filter (fn [x] (> arabic x)))
       max))

(to-roman 14) ;; adf
(to-roman 1)
