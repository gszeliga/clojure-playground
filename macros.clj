;;Sample macro showing an how an imperative 'foreach' would look like 
(defmacro foreach [[sym coll] & body]
  `(loop [coll# ~coll]
     (when-let [[~sym & xs#] (seq coll#)]
       ~@body
       (recur xs#))))

(macroexpand-1 '(foreach [x [1 2 3]]
                     (println x)))

(foreach [x [1 2 3]]
         (println x))



(macroexpand-1 foreach)

;;First example of a stupid macro which reverts symbol before compilation
(require '(clojure [string :as str]
                   [walk :as walk]))

(defmacro reverse-it
  [form]
  (walk/postwalk #(if (symbol? %)
                    (symbol (str/reverse (name %)))
                    %)
                 form))

(reverse-it 
  (qesod [gra (egnar 5)]
         (nltnirp (cni gra))))

(macroexpand-1 '(reverse-it 
                  (qesod [gra (egnar 5)]
                         (nltnirp (cni gra)))))


(defmacro spy-env []
  (let [ks (keys &env)]
    `(prn (zipmap '~ks [~@ks]))))

(let [x 1 y 2]
  (spy-env)
  (+ x y))

(defmacro simplify
  [expr]
  (let [locals (set (keys &env))]
    (if (some locals (flatten expr))
      expr
      (do
          (println "Precomputing: " expr)
          (list `quote (eval expr))))))

(defn f
  [a b c]
  (+ a b c (simplify (apply + (range 5e7)))))

(f 1 2 3)

(@#'simplify nil {} '(inc 1))

(defn macroexpand1-env [env form]
  (if-let [[x & xs] (and (seq? form) (seq form))]
    (if-let [v (and (symbol? x) (resolve x))]
      (if (-> v meta :macro)
        (apply @v form env xs)
        form)
      form)
    form))
