(defn i-integers
  ([] (i-integers 0))
  ([n] (list n (fn [] (i-integers (inc n))))))

(defn ifirst [l] (first l))
(defn irest [l] ((second l)))

(def t (i-integers))

(ifirst (irest (irest t)))
;; => 2

(defn inth [col index]
  (loop [idx 0
         c col]
    (if (= idx index)
      (ifirst c)
      (recur (inc idx) (irest c)))))

(inth t 4)
;; => 4

(defn i-range [start fin]
  (if (> start fin)
    '()
    (list start (fn [] (i-range (inc start) fin)))))

(defn iforeach [col proc]
  (loop [c col]
    (if (ifirst c)
      (do (proc (ifirst c))
          (iforeach (irest col) proc)))))

(iforeach (i-range 5 10) (fn [x] (println (str x))))
;; => "5 6 7 8 9 10"

(defn iprint [col]
  (iforeach col (fn [x] (println (str x)))))

(def r (i-range 5 10))
(iprint r)

