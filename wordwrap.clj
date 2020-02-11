(ns katas.wordwrap
  (:require [clojure.test :refer :all]))

(defn trace [str x]
  (println str x)
  x)

(defn words [s]
  (clojure.string/split s #" "))

(defn wrap [input width]
  (reverse (first (reduce (fn [[response remainingWidth] nextWord]
                           (let [wordLength (count nextWord)
                                 currentLine (last response)]
                             (if (>= remainingWidth wordLength)
                               [(conj (drop-last response)
                                      (str currentLine nextWord))
                                (- remainingWidth wordLength)]
                               [(conj response nextWord) width]
                               )))
                         [[""] width]
                         (words input)))))


(deftest wordWrapTest
  (testing "wraps at boundaries"
    (is (= ["test"] (wrap "test" 4)))
    (is (= ["te" "st"]  (wrap "te st" 2)))
    )

  )

(run-tests)
