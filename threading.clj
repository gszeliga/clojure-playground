(defn ensure-seq [x]
  (if (seq? x) x (list x)))

;First attempt
(defn insert-second
  "Insert x as the second item in seq y."
  [x ys]
  (let [ys (ensure-seq ys)]
    (concat (list (first ys) x) 
            (rest ys))))

;simplified version using macros
(defn insert-second
  "Insert x as the second item in seq y."
  [x ys]
  (let [ys (ensure-seq ys)]
  `(~(first ys) ~x ~@(rest ys))))

;even more simplified version just creating a list from lists
(defn insert-second
  "Insert x as the second item in seq y."
  [x ys]
  (let [ys (ensure-seq ys)]
  (list* (first ys) x (rest ys))))

(defmacro thread
  "Thread x through successive forms."
  ([x] x)
  ([x form] (insert-second x form))
  ([x form & more] `(thread (thread ~x ~form) ~@more)))

(thread [1 2 3] (conj 4) reverse println)

(require '[clojure.walk :as walk])
(walk/macroexpand-all `(thread [1 2 3] (conj 4) reverse println))
