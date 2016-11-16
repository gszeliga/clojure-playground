(require '[clojure.java.io :as io])

(def console (agent *out*))
(def character-log (agent (io/writer "character-states.log" :append true)))

(defn write 
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
  (doto w 
    (.write "\n")
    (.flush)))

(defn log-reference
  [reference & writer-agents]
  (add-watch reference :log
             (fn [_ reference old new]
               (doseq [writer-agent writer-agents]
                 (send-off writer-agent write new)))))
