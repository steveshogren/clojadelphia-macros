









;; what was my problem?






(def p1 {:id 1 :name "Sam"})

(defn toString [p]
  (str (:id p) (:name p)))

{:a 1 :b 2 :c  "here how" :d "there we go now" :e "time to file" :f "this that " :g 44444 :h 2}

(toString p1)
;; => 1Sam


(def p1 {:id 1 :name "Sam"})
(toString p1)
;; => 1





;; Now it just doesn't show the name, no issue...






;; We talked actually in the 2.1 session of SICP about a way around this:

(defn person-get-id [p] (:id p))
(defn person-get-name [p] (:name p))
(defn toString [p]
  (str (person-get-id p) (person-get-name p)))

(toString p1)

;; uh-oh










;; I wanted a runtime error when this happens, and a programmatic
;; way to see the places I needed to update





;; let's start simple, I needed a "shape" for my map
(def Person [:id :name])






;; We need a way to see if a map "is-a" Person
(defn is-type [coll type]
  (if (vector? type) 
    (if (empty? type)
      true
      (reduce (fn [iret k]
                (and iret
                     (contains? coll k)))
              true
              type))
    (instance? type coll)))

(def DontCheck [])
(is-type 1 Long)   ;; => true
(is-type "" Long)  ;; => false
(is-type 1 DontCheck)     ;; => true
(is-type 1 [])            ;; => true

(is-type {:id 1 :name "Sam"} Person) ;; => true
(is-type {:id 1} Person)             ;; => false















;; Now how to change a function to do what we want?
;; What is my desired syntax?

(defn toString [p Person] String
  (str (:id p) (:name p)))












;; What would this output?
(defn toString [p]
  {:pre [(is-type p Person)]
   :post [(is-type % String)]}
  (str (:id p) (:name p)))



(toString {:id 1 :name "Sam"})
(toString {:id 1 :firstname "Sam"})
;; Holy Runtime Errors Batman!





;; Let's start the macro

(defmacro deft [name & res]
  `(defn ~name ~res))

(deft this [x y] (+ x y))
(this 1 2)
(mprint '(deft this [x y] (+ x y)))





(deft toString [p Person] String
  (str (:id p) (:name p)))
(toString p1)

([p Person] String (str (:id p) (:name p)))

(defn toString [p]
  {:pre [(is-type p Person)]
   :post [(is-type % String)]}
  (str (:id p) (:name p)))























;; we want to add a shape to these
(defmacro deft [name & res]
  (tracelet [paramlist (first res)
             return-type (second res)
             params (getParameters paramlist)
             pre (getPre paramlist)
             prepost (addPost pre return-type)
             body (drop 2 res)]
       `(defn ~name ~params ~prepost ~@body)))
(mprint '(deft test [a Person] [] (+ 1 1)))
(deft test [a Person] [] (+ 1 1))



;; (getParameters ['a 1 'b 2 'c 3])
;; => (a b c)
(defn getParameters [res]
  (vec (map first (partition 2 res))))

;; (getPre ['a 't1 'b 't2 'c 't3])
;; => {:pre [(is-type a t1) (is-type b t2) (is-type c t3)]}
(defn getPre [res]
  {:pre
   (vec (map #(list 'is-type (first %) (second %))
             (partition 2 res)))})
(defn addPost [pre res]
  (conj pre {:post [(list 'is-type '% res)]}))


(deft toString [p Person] String
  (str (:id p) (:name p)))

(def p1 {:id 1 :name "Sam"})

(toString p1)












;; Macro Helpers

(defn wrap-args-with-trace [[symb val]]
  [symb (list `trace (str "let-" symb) val)])

(defmacro tracelet [args & body]
  (let [arg-pairs (partition 2 args)
        new-bindings (vec (mapcat wrap-args-with-trace arg-pairs))]
    `(let ~new-bindings ~@body)))
(pprint (macroexpand '(tracelet [a 1 b 2] a)))


(defn mprint [m]
  (pprint (macroexpand m)))

(defmacro trace [l v]
  `(do (println ~l (str ~v)) ~v))

(trace "this" (+ 1 1))
