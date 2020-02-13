(ns katas.dfs
  (:require [clojure.test :refer :all]))

(defn trace [str x]
  (println str x)
  x)

(defn insert [tree v]
  (if (= 0 (count tree))
    [v [] []]
    (let [[node l r] tree]
      (if (< v node)
        [node (insert l v) r]
        ))
    )
  )

(deftest dfsTest
  (testing "make a tree"
    (is (= [1 [] []] (insert [] 1)))
    (is (= [3 [2 [] []] []] (insert (insert [] 3)
                               2)))
    )

  )

(run-tests)
