(defn character
  [name & {:as opts}]
  (ref (merge {:name name :items #{} :health 500}
              opts)))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(defn loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (alter to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))    

(loot smaug bilbo)
(deref bilbo)

(defn attack
  [aggressor target]
  (dosync 
    (let [damage (* (rand 0.1) (:strength @aggressor))]
     (commute target update-in [:health] #(max 0 (- % damage)))))) 

(defn heal
  [healer target]
  (dosync 
    (let [aid (* (rand 0.1) (:mana @healer))]
      (when (pos? aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (commute target update-in [:health] + aid)))))


(attack smaug bilbo)
(heal gandalf bilbo)

(defn enforce-max-health
  [{:keys [name health]}]
  (fn [character-data]
   (or (<= (:health character-data) health)
      (throw (IllegalArgumentException. (str name " is already at max length")))))) 

(defn character
  [name & {:as opts}]
  (let [cdata (merge {:name name :items #{} :health 500}
                     opts)
        cdata (assoc cdata :max-health (:health cdata))
        validators (list* (enforce-max-health name (:health cdata))
                         (:validators cdata))]
    (ref (dissoc cdata :validators) 
         :validator #(every? (fn [v] (v %)) validators))))
