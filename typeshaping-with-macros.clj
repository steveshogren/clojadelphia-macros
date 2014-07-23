;; what was my problem?

(def p1 {:id 1 :name "Sam"})

(defn toString [p]
  (str (:id p) (:name p)))

(toString p1)
;; => 1Sam




(def p1 {:id 1 :firstname "Sam"})
(toString p1)
;; => 1

;; Now it just doesn't show the name, no issue...








;; I wanted a runtime error for things like this, and a programmatic
;; way to see the places I needed to update





;; let's start simple, I needed a "shape" for my map
(def Person [:id :name])






;; We need a way to see if a map "is-a" Person
(defn is-type [coll type]
  (if (vector? type) 
    (reduce (fn [iret k]
              (and iret
                   (contains? coll k)))
            true
            type)
    (instance? type coll)))

(is-type 1 Long)   ;; => true
(is-type "" Long)  ;; => false

(is-type {:id 1 :name "Sam"} Person) ;; => true
(is-type {:id 1} Person)             ;; => false















;; Now how to change a function to do what we want?
;; What is my desired interface?

(defn toString [p Person] String
  (str (:id p) (:name p)))







;; What would this output?
(defn toString [p]
  {:pre [(is-type p Person)]
   :post [(is-type % String)]}
  (str (:id p) (:name p)))

(toString {:id 1 :firstname "Sam"})
;; Holy Runtime Errors Batman!







;; Let's start the macro
(defmacro deft [name & res] `(defn ~name ~res))

(deft this [x y] (+ x y))
(this 1 2)



;; we want to add a shape to these
(defmacro deft [name & res]
  (let [names-n-types (partition 2 res)]
    `(defn ~name ~res)))

(defn getParameters [res])
(defn getPrePost [res])







