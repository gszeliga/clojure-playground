(declare empty-array-set)
(def ^:private ^:const max-size 4)

(deftype ArraySet [^objects items
                   ^int size
                   ^:unsynchronized-mutable ^int hashcode]
  clojure.lang.IPersistentSet
  (get [this x]
    (loop [i 0]
      (when (< i size)
        (if (= x (aget items i))
          (aget items i)
          (recur (inc i))))))

  (contains [this x]
    (boolean
      (loop [i 0]
        (when (< i size)
          (or (= x (aget items i)) (recur (inc i)))))))

  (disjoin [this x]
    (loop [i 0]
      (if (== i size)
        this
        (if (not= x (aget i items))
          (recur (inc i))
          (ArraySet. (doto (aclone items)
                       (aset i (aget items (dec size)))
                       (aset (dec size) nil))
                     (dec size)
                     -1)))))

  clojure.lang.IPersistentCollection
    (count [this] size)
    (cons [this x]
      (cond
        (.contains this x) this
        (== size max-size) (into #{x} this)
        :else (ArraySet. (doto (aclone items)
                          (aset size x))
                         (inc size)
                         -1)))
    (empty [this] empty-array-set)
    (equiv [this that] (.equals this that))
 
  clojure.lang.Seqable
  (seq [this] (take size items))
    
  Object
  (hashCode [this]
    (when (== -1 hashcode)
      (set! hashcode (int (areduce items idx ret 0
                                   (unchecked-add-int ret (hash (aget items idx))))))))
  (equals [this that]
    (or
     (identical? this that)
     (and (or (instance? clojure.lang.IPersistentSet that)
              (instance? java.util.Set))
          (= (count this) (count that))
          (every? #(contains? % this) that))))


  java.util.Set
  (isEmpty [this] (zero? size))
  (size [this] size)
  (toArray [this array]
    ;'sequences' are java collections
    (.toArray ^java.util.Collection (sequence items) array))
  (toArray [this] (into-array (seq this)))
  (iterator [this] (.iterator ^java.util.Collection (sequence this)))
  (containsAll [this coll]
    (every? #(contains? % this) coll))

  clojure.lang.IFn
  (invoke [this key] (.get this key))
  (applyTo [this args]
    (when (not= 1 (count args))
      (throw (clojure.lang.ArityException. (count args) "ArraySet"))
      (this (first args)))))


  (def ^:private empty-array-set (ArraySet. (object-array max-size) 0 -1))

  (defn array-set
    "Creates an array-backed set containing the given values."
    [& vals]
    (into empty-array-set vals))



;Some usage samples
(array-set)
(conj (array-set) 1)
(apply array-set "hello")
(get (apply array-set "hello") \w)
(get (apply array-set "hello") \h)
(contains? (apply array-set "hello") \h)
(= (array-set) #{})
((apply array-set "hello") \h)
