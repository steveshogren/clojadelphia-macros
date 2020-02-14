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
        [node l (insert r v)]))))

(defn delete [tree v]
  (if (= [] tree) tree
      (let [[node l r] tree]
        (if (= node v)
          (cond
            (and (= l []) (= r [])) []
            (not= l []) (let [[lv ll lr] l]
                          [lv ll r])
            true (let [[rv rl rr] r]
                          [rv l rr])
            )
          (if (< v node)
            [node (delete l v) r]
            [node l (delete r v)]
            ))
          )
        )
  )

(deftest dfsTest
  (testing "delete from tree"
    (is (= [3 [] []] (delete [3 [2 [] []] []] 2)))

    (is (= [6
            [4
             []
             []]
            []]
           (delete [6
                    [5
                     []
                     [4 [] []]]
                    []]
                   5)))

    (is (= [6
            [2
             []
             [4 [] []]]
            []]
           (delete [6
                    [5
                     [2 [] []]
                     [4 [] []]]
                    []]
                   5)))

    (is (= [] (delete [] 2)))
    )
  (testing "make a tree"
    (is (= [1 [] []] (insert [] 1)))
    (is (= [3
            [2 [] []]
            []]
           (insert (insert [] 3) 2)))
    (is (= [3
            []
            [4 [] []]]
           (insert (insert [] 3) 4)))
    ))

(run-tests)
