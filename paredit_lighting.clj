;; Expect more from your editor!




;; Adding opening always closes, and vice versa
()


;; Never unbalanced



;; Rainbow parens:
((((((()))))))




;; Grow shrink to remove let binding
(defn add [x y]
  (+ x y))

;; open line
;; paredit-raise-round
(defn add [x y]
 ( 
  (+ x y)))

;; text
(defn add [x y]
 (let []
  (+ x y)))

;; paredit-forward-slurp-sexp
(defn add [x y]
  (let [(+ x y)]))


;; 2x paredit-forward
;; text
(defn add [x y]
  (let [sum (+ x y)]
    sum))
