(ns katas.wordwrap
  (:require [clojure.test :refer :all]))

(defn wrap [input width]
  (loop [input input
         remainingWidth input]
    (if )))

(deftest wordWrapTest
  (testing "wraps at boundaries"
    (is (= ["test"] (wrap "test" 4)))
    (is (= ["te" "st"]  (wrap "te st" 2)))
    )

  )

(run-tests)
