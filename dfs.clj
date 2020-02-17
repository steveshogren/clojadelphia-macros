(ns katas.dfs
  (:require [clojure.test :refer :all]))

(defn trace [str x]
  (println str x)
  x)

(defn set-left [v newLeft]
  (let [[node l r] (if (vector? v) v [v [] []])]
    [node newLeft r]))

(defn set-right [v newRight]
  (let [[node l r] (if (vector? v) v [v [] []])]
    [node l newRight]))


(defn insert [tree v]
  (let [newNode (if (vector? v) v [v [] []])
        [vv vl vr] newNode]
    (if (= 0 (count tree))
      newNode
      (let [[node l r] tree]
        (if (< vv node)
          (set-left tree (insert l newNode))
          (set-right tree (insert r newNode))
          )))))

(defn delete [tree v]
  (let [newNode (if (vector? v) v [v [] []])
        [vv vl vr] newNode]
    (if (= [] tree) tree
        (let [[node l r] tree]
          (if (= node vv)
            (cond
              (and (= l []) (= r [])) []
              (not= l []) (let [[lv ll lr] l]
                            (if (= [] lr)
                              [lv ll r]
                              [lv ll (insert r lr)]
                              ))
              true (let [[rv rl rr] r]
                     [rv l rr])
              )
            (if (< vv node)
              [node (delete l newNode) r]
              [node l (delete r newNode)]
              ))
          )
        ))
  )

(defn pretty-print [tree]
  (loop [nodes []
         queue [tree]
         total-depth 0]
    (let [tree (first queue)
          newNode (if (and (vector? tree)
                           (not= [] tree)) tree [tree [] []])
          [vv vl vr] newNode]
      (if (= [] vv)
        nodes
        (do
          (recur (conj nodes vv)
                 (conj (conj queue vl) vr)
                 (inc total-depth)
                 )
            ))))
  )

;; (pretty-print [5 [2 [] []] [9 [] []]])

;;(pretty-print [6 [5 [2 [] []] [4 [] []]] []])


(deftest dfsTest
  (testing "delete from tree"
    (is (= [3 [] []] (delete [3 [2 [] []] []] 2)))

    ;; promotes right child if no left
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

    ;; promotes left child if exists
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

    ;; balances children by moving left child's right node to right
    (is (= [7
            [2
             [1 [] []]
             [6 [3 [] []] []]]
            []]
           (delete [7
                    [5
                     [2 [1 [] []] [3 [] []]]
                     [6 [] []]]
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
