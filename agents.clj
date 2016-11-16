(def a (agent 42))

(reduce send a (for [x (range 33)]
                 (fn [_] (throw (Exception. (str "error #" x))))))

(agent-error a)
(restart-agent a 42)
(agent-error a)
(restart-agent a 42 :clear-actions true)

(def b (agent nil
              :error-mode :continue
              :error-handler (fn [the-agent exception]
                               (.println System/out (.getMessage exception)))))

(send b (fn [_] (throw (Exception. "failing!"))))
(agent-error b)


