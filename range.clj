(ns katas.range
  (:require [clojure.test :refer :all]))

(defn contains_ [[xNeedle yNeedle] [a b]]
   (and (<= yNeedle b) (<= a xNeedle)))

(defn range_ [a b]
  [a b])

(deftest rangeTest
  (testing ""
    (is (= true (contains_ (range_ 2 5) (range_ 1 7))))
    (is (= false (contains_ (range_ 0 2) (range_ 1 7))))
    (is (= false (contains_ (range_ 2 9) (range_ 1 7))))
    (is (= true (contains_ (range_ 1 1) (range_ 1 7))))
    (is (= true (contains_ (range_ 1 7) (range_ 1 7))))
    ))

(run-tests)
