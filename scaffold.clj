(defn scaffold
  "Given an interface, returns a 'hollow' body suitable for use with 'deftype'."
  [interface]
  (doseq [[iface methods] (->> interface
                               .getMethods
                               (map #(vector (.getName (.getDeclaringClass %))
                                             (symbol (.getName %))
                                             (count (.getParameterTypes %))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
               (str "    "
                    (list name (into '[this] (take argcount (repeatedly gensym)))))))))

(scaffold clojure.lang.IPersistentSet)
