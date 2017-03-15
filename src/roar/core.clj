(ns roar.core  (:gen-class) )
(require '[roar.protocol :as proto])

(defprotocol Node
  (write!       [this k v])
  (read!        [this k])
  (find-in-keys!   [this pattern])
  (find-in-values! [this pattern])
  )

(defprotocol MasterNode
  (add-slave! [this slave]))

(defprotocol ReadWrite
  (write-key!     [this k v])
  (read-key       [this k])
  (replicate-key! [this k v])
  (match-key!     [this pattern])
  (match-value!   [this pattern])
  )

(defrecord Master [rw-strategy name slaves])
(defrecord Slave  [rw-strategy name master])

(defrecord InMemoryReadWrite [store])

(extend-type InMemoryReadWrite ReadWrite
  (write-key!
    [this k v]
    (-> (:store this)
        (swap! conj {(keyword k) v})))
  (read-key
    [this k]
    (-> @(:store this)
        ((keyword k))))
  (replicate-key!
    [this k v]
    (-> (:store this)
        (swap! conj {(keyword k) v})))
  (match-key!
    [this pattern]
    (-> @(:store this)
        ((keyword pattern))
        ;(doseq [[key val] :store] (prn key val))
        ))
  (match-value!
    [this pattern]
    (-> @(:store this)
        ((str pattern)))
    )
  )
;)

(extend-type Master Node
  (write!
    [this k v]
    (-> (:rw-strategy this)
        (write-key! k v))
    (let [slaves @(:slaves this)]
      (doseq [{:keys [rw-strategy]} slaves]
        (replicate-key! rw-strategy k v)))
    this)
  (read!
    [this k]
    (-> (:rw-strategy this)
        (read-key (keyword k))))
  (find-in-values!
    [this pattern]
    (set (filter
           (fn [key]
             (re-matches
               (re-pattern pattern)
               (name key)
               ))
           (map (fn [[k v]] v ) @(-> this :rw-strategy :store))
           )))
  (find-in-keys!
    [this pattern]
    (sync nil (set (filter
                     (fn [key]
                       (re-matches
                         (re-pattern pattern)
                         (name key)
                         ))
                     (keys @(-> this :rw-strategy :store) )
                     )))))


(extend-type Master MasterNode
  (add-slave!
    [this slave]
    {:pre [(and (satisfies? Node slave)
                (not (satisfies? MasterNode slave)))]}
    (-> this
        :slaves
        (swap! conj (assoc slave :master this)))
    (-> slave
        :master
        (reset! this))
    (let [store @(:store (:rw-strategy this))]
      (doseq [[k v] store]
        (replicate-key! (:rw-strategy slave) (name k) v)))
    this))

(extend-type Slave Node
  (write!
    [this k v]
    (-> @(:master this)
        (:rw-strategy)
        (write-key! k v)))
  (read!
    [this k]
    (-> (:rw-strategy this)
        (read-key (keyword k))))

  (find-in-keys!
    [this pattern]
    (-> (:rw-strategy this)
        (match-key! (keyword pattern))))
  (find-in-values!
    [this pattern]
    (let [slaves @(:slaves this)]
      (doseq [{:keys [rw-strategy]} slaves]
        (match-value! rw-strategy pattern))))

  )

(defn master-node
  [name]
  (Master. (InMemoryReadWrite.  (atom {})) name (atom [])))


(defn slave-memory-node
  ([name]
   (Slave. (InMemoryReadWrite. (atom {})) name (atom {})))
  ([name master]
   {:pre [(satisfies? MasterNode master)]}
   (Slave. (InMemoryReadWrite. (atom {})) name (atom master))))


(def master  (roar.core/master-node "master"))
(def memory1 (roar.core/slave-memory-node "memory1"))
(def memory2 (roar.core/slave-memory-node "memory2"))

(roar.core/add-slave! master memory1)
(roar.core/add-slave! master memory2)

(defn execute
  [packet]
  (let
    [
     command (roar.protocol/get-command-type (-> packet :command))
     key     (-> packet :data :key)
     value   (-> packet :data :value)
     ]
    (cond
      (= command :get) ( read!  master key )
      (= command :set) ( write! master key value )
      (= command :match-key)   ( find-in-keys!    master key )
      (= command :match-value) ( find-in-values!  master key )
      :else nil
      )
)
)

(def buffer (atom ""))

(defn checkCmd [cmd]
  (= cmd "`*`*`")
  )

(defn process [cmd]
  (println
    (str
      (execute
        (roar.protocol/parse-frame
          (str @buffer cmd))))
    )
  (reset! buffer "")
  )

(defn -main
  []
  (let [ cmd (read-line) ]
    (if (checkCmd cmd)
      (process cmd)
      (swap! buffer str cmd))
    )
  (recur))
