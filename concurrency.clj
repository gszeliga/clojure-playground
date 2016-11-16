(def d (delay (println "Running...") :done!))

(deref d)

(def long-calculation (future (apply + (range 1e8))))

(deref long-calculation)
(println @long-calculation)

@(future (Thread/sleep 5000) :done!)

(def p (promise))
(realized? p)
(deliver p 42)
(realized? p)
(deref p)
(conj [1 2])

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))


(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

@(atom 12)

(def sarah (atom {:name "Sarah" :age 25 :wears-glasses? false}))

(swap! sarah update-in [:age] + 3)

(swap! sarah (comp #(update-in % [:age] inc)
                   #(assoc % :wears-glasses? true)))

(def xs (atom #{1 2 3}))

(wait-futures 1 (swap! xs (fn [v]
                         (Thread/sleep 250)
                         (println "trying 4")
                         (conj v 4)))
              (swap! xs (fn [v]
                       (Thread/sleep 500)
                       (println "trying 5")
                       (conj v 5))))

(deref xs)

(defn echo-watch
  [key identity old new]
  (println key old "=>" new))

(def sarah (atom {:name "Sarah" :age 25}))
(add-watch sarah :echo echo-watch)
(swap! sarah update-in [:age] inc)

