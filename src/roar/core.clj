(ns roar.core
  (:refer-clojure :exclude [read])
  )

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
(defrecord OnDiskReadWrite   [store])

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
    ()
    ;(-> @(:store this)
    ;    ((map #( (prn )) seq))
    ;    )
    )
  )


; http://cr.yp.to/cdb.html
(extend-type OnDiskReadWrite ReadWrite
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
    ()
    ;(-> @(:store this)
    ;    ((map #( (prn )) seq))
    ;    )
    )
  )

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
  (find-in-keys!
    [this pattern]
    (-> (:rw-strategy this)
        (match-key! pattern)))
  (find-in-values!
    [this pattern]
    (let [slaves @(:slaves this)]
      (doseq [{:keys [rw-strategy]} slaves]
        (match-value! rw-strategy pattern))))
  )

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

(defn slave-disk-node
  ([name]
   (Slave. (OnDiskReadWrite (atom {})) name (atom {})))
  ([name master]
   {:pre [(satisfies? MasterNode master)]}
   (Slave. (OnDiskReadWrite. (atom {})) name (atom master))))

(def master  (roar.core/master-node "master"))
(def memory1 (roar.core/slave-memory-node "memory1"))
(def memory2 (roar.core/slave-memory-node "memory2"))
(def disk1   (roar.core/slave-disk-node   "disk2"))

(roar.core/add-slave! master memory1)
(roar.core/add-slave! master memory2)
(roar.core/add-slave! master disk1)

(roar.core/write! master "key" "test23")

(= (roar.core/read! memory1 "key") "test23")
(= (roar.core/read! memory2 "key") "test23")
(= (roar.core/read! disk1   "key") "test23")
(= (roar.core/read! master  "key") "test23")
