(defprotocol Measurable
  "A protocol for retrieving the dimensions of widgets."
  (width [measurable] "Returns the width in px.")
  (height [measurable] "Returns the height in px."))

(defrecord Button [text])

(extend-type Button
  Measurable
  (width [btn]
    (* 8 (-> btn :text count)))
  (height [btn] 8))

(def bordered 
  {:width #(* 2 (:border-width %))
   :height #(* 2 (:border-height %))})

(defn combine
  [op f g]
  (fn [& args]
    (op (apply f args) (apply g args))))

(defrecord BorderedButton [text border-width border-height])

(extend BorderedButton
  Measurable
  (merge-with (partial combine +)
              (get-in Measurable [:impls Button])
              bordered))

(let [btn (Button. "Hello World!")]
  [(width btn) (height btn)])

(let [bbtn (BorderedButton. "Hello Worlds!" 6 4)]
  [(width bbtn) (height bbtn)])

