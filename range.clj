(ns katas.range
  (:require [clojure.test :refer :all]))

(defn contains_ [points [a b]]
  (reduce (fn [ret x] (and ret (and (<= x b) (<= a x))))
          true
          points))

(defn range_ [a b]
  [a b])

(defn allPoints_ [[a b]]
  (range a (inc b))
  )

(deftest rangeTest
  (testing "all points"
    (is (= [1 2 3] (allPoints_ (range_ 1 3))))
    )
  (testing "contains_"
    (is (= true (contains_ [2 5] (range_ 1 7))))
    (is (= false (contains_ [0 2] (range_ 1 7))))
    (is (= false (contains_ [2 9] (range_ 1 7))))
    (is (= true (contains_ [1 1] (range_ 1 7))))
    (is (= true (contains_ [7] (range_ 1 7))))
    ))

(run-tests)
