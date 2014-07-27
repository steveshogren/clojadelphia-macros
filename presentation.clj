




;; The blog post that inspired this talk:
      deliberate-software.com/intro-to-macros
;; This file is stored code at:
      github.com/steveshogren/clojadelphia-macros
;; Inspired from the book:
      Let Over Lambda by Doug Hoyte
      












;; Templates

;; Think about templates a second...
<html>
  <p>
  <?php> if(today_day() == "Monday") { echo("Mon") } </?>
  </p>
</html>

<html>
  <p>
  <% if todayDay() eq "Monday" then return "Mon" end %>
  </p>
</html>

;;They let you "disable/enable" the evaulation of code,
;; the template is what is returned, but first the code
;; blocks are evaluated, allowing for expansion of the template

<html>
  <ul>
  <% foreach (var person in people) {  %>
     <li> <% person.name %> </li>
  <% } %>
  </ul>
</html>











;; no quote                +   => #<core$_PLUS_ clojure.core$_PLUS_@d3d424c>
;; ' => quote             '+   => +

;; ` => syntax quote      `+   => clojure.core/+
;; ~ => unquote           `~+  => #<core$_PLUS_ clojure.core$_PLUS_@d3d424c>
;; ~@ => unquote splicing
;;    `(~@[1 3] 1) => (1 3 1)
;;    `(~@[+ 3] 1) => (#<core$_PLUS_ clojure.core$_PLUS_@d3d424c> 3 1)

;; Function
(defn add [x y]
  (+ x y))
;; (add (+ 1 1) 3) => 5

;; "Replaced"
(defn add [2 3]
  (+ 2 3))
; 

;; Macro 
(defmacro add [x y]
  `(+ ~x ~y))
;; (add (+ 1 1) 3) => 5

;; After "replacing" the values
(defmacro add [(+ 1 1) 3]
  `(+ (+ 1 1) 3))
;; (pprint (macroexpand '(add (+ 1 1) 3))) => (clojure.core/+ (+ 1 1) 3)
;; Notice the inner + is not namespaced, because it was "passed in"
;; assuming it was defined in the local namespace


;; Macros allow for the expansion of code as if there was
;; no increase in stack depth














;; ---- Game probability macros ----

(defn chance [x]
  (> x (rand-int 100)))

(average-out #(if (chance 50) 1 100) 10000)

(defmacro make-percents []
  "(ifN x y) returns x N% of the time, but ensures conditional evaluation, like 'if'"
  `(list ~@(map (fn [num] 
                  (let [macro-name (symbol (str "if" num))]
                    `(defmacro ~macro-name [x# y#]
                       `(if (chance ~~num) ~x# ~y#))))
                (range 1 100))))
;
;;(make-percents)
;; (pprint (macroexpand '(make-percents))) => (defmacro if0 ...) (defmacro if1 ...) ...
;; (if99 1 100)
;; (pprint (macroexpand '(if1 :x :y)))
;; (average-out #(if50 1 100) 10000)   




;; But this function is lame!



;; Think about what you want it to look like _first_

(if-chance "50%" :a "45%" :b "5%" :c)

(if-percent 50 (get-db), 45 :b, 5 :c)

(if-percent-b [50 :a] [45 :b] [5 :c])
;
(if-percent-c :a 50 :b 45 :c 5)

(return-a 50 % chance of :a
          45 % chance of :b
          5 % chance of :c)

(if-percent ********** :a
            *********  :b
            *          :c)












            
;; So these are cool, but how do we make them work on seqs?
(defmacro adder [x y]
  `(+ ~x ~y))

(reduce  #(adder %1 %2) 0 [4 5])
;; => java.lang.RuntimeException: Can't take value of a macro: #'user/adder

;
;; We make a regular function syntax that might be unwieldy, but allowing at
;; least the _ability_ to use over collections
(if-percent-fn 50 (fn [] :a)
               49 (fn [] :b)
               1 (fn [] :c))













(defn if-percent-fn [& n]
  " Takes a set of percent chances and return functions
    to call if that 'chance' happens. All percents must
    add up to 100, but any number are supported.
   (if-percent-fn 50 (fn [] :a)
                  49 (fn [] :b)
                  1 (fn [] :c))
    => returns :a 50% of the time, :b 49%, and :c 1%"
  (if (odd? (count n))
    (throw (Exception. "Must pass even num of args"))
    (let [pairs (partition 2 n)
          sum (reduce + (filter pos? (map first pairs)))]
      (if (= 100 sum)
        (let [roll (inc (rand-int 100))]
          (loop [current-sum 0
                 items pairs]
            (let [[next-percent next-val] (first items)
                  current-sum (+ current-sum next-percent)]
              (if (<= roll current-sum)
                (next-val)
                (recur current-sum (rest items))))))
        (throw (Exception. (str "Nums: " sum " didn't equal 100" )))))))

;; This frees us now to explore shorter or more expressive syntaxes








(defmacro if-percent [& n]
  "(if-percent 50 :a 49 :b 1 :c) returns :a 50% of the time, :b 49%, and :c 1%"
  (if (even? (count n))
    (let [pairs (partition 2 n)
          pairs (mapcat (fn [[percent return]]
                          [percent `(fn [] ~return)])
                        pairs)]
      `(if-percent-fn ~@pairs))))



(pprint (macroexpand '(if-percent 50 1 50 100))) 
(average-out #(if-percent 50 1 50 100) 10000)   
(average-out #(if-percent ************** 1 ****** 100) 10000)    












;; So, while this star syntax is cute, it has a downside: now
;; we cannot pass bound vals to the macro! 

(let [seventy 70
      thirty 30]
  (if-percent seventy 1
              thirty 100))
;; => java.lang.Exception: Nums: 65 didn't equal 100


(let [seventy 70
      thirty 30]
  (if-percent-fn seventy (fn [] 1)
                 thirty (fn [] 100)))
;; => Works!

;; So this is the converse power of macros, a neat DSL, while
;; nice looking, can limit you in surprising ways, and otherwise
;; make your API actually _harder_ to use






















;; ---- Anaphoric Macros ----



(let [x 1]
  (let [x 4]
    x)
  x)



;; Here we shadow the z value with another
(defmacro add-weird [x]
  `(let [~'z 100]
     ~x))

(let [z 1]
   (add-weird (inc z)))
;; => 101 ...not 2 

(clojure.walk/macroexpand-all
 '(let [z 1]
   (add-weird (inc z))))
;; => (let* [z 1] (let* [z 100] (inc z)))



;; Anaphoric macros allow the deliberate shadowing of
;; values when expanded. While dangerous, this can be
;; very useful in certain circumstances




;; _> is meant to "enhance" the value of the normal ->
;; command by allowing a Scala-like syntax
(defmacro _> [init & body]
  "Used to anaphorically replace _ with the result of the previous expr
   (_> 1 (+ 4 _) (+ _ 2) (* _ _)) => 49 
   (_> (+ 1 1) (* _ _)) => 4 " 
  `(let [~'_ ~init
         ~@(mapcat (fn [x] `[~'_ ~x])
                   body)]
     ~'_))


#_(_> (range 1000)
      (map (fn [x] (if-percent 50 1 50 100)) _)
      (average _) 
      (float _))


(pprint (macroexpand
         '(_> (range 1000)
              (map (fn [x] (if-percent 50 1 50 100)) _)
              (average _) 
              (/ _ 3)
              (float _))))


;; Transliteration of PG's aif
;; Anaphoric macro that binds to "it" the test result
(defmacro aif [test then else]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))

#_(aif (+ 1 1) 
        it
       :some-return)
;;=> 2

;; Transliteration of PG's alambda
;; Anaphoric macro that binds to "self" the function for recursion
;; in typical Clojure, this should be achieved with loop/recur
(defmacro alambda [parms & body]
  `(letfn [(~'self ~parms ~@body)]
     ~'self))



#_((alambda [n]
     (if (> n 0)
       (cons
        n
        (self (- n 1))))) 5)


(defn get-av []
  (average [1 2 3 4 5]))

(defn average [col] 
  (let [sum (reduce + col),
        count (count col)]
    (/ sum count)))

(defn average [col] 
  (/ (reduce + col)
     (count col)))

(defn average-out [func times]
  (->> (range times)
       (map (fn [x] (func)))
       average 
       float))

 
