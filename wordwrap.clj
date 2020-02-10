(ns katas.wordwrap
  (:require [clojure.test :refer :all]))

(defn trace [str x]
  (println str x)
  x)

(defn words [s]
  (clojure.string/split s #" "))

(defn wrap [input width]
  (first (reduce (fn [[response remainingWidth] nextWord]
                  (let [wordLength (count nextWord)
                        currentLine (last response)]
                    (trace "word length" wordLength)
                    (trace "response" response)
                    (if (>= remainingWidth wordLength)
                      [(trace "first ret" (conj (drop-last response)
                                                (str currentLine nextWord)))
                       (- remainingWidth wordLength)]
                      [(trace "new line" (conj response [nextWord]))
                       width]
                      )))
                [[""] width]
                (words input))))

(deftest wordWrapTest
  (testing "wraps at boundaries"
    (is (= ["test"] (wrap "test" 4)))
    (is (= ["te" "st"]  (wrap "te st" 2)))
    )

  )

(run-tests)
