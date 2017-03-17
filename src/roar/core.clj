(ns roar.core)
(require '[roar.protocol :as proto] )

(defprotocol Node
  (write!       [this data])
  (read!        [this data])
  (find-in-keys!   [this pattern])
  (find-in-values! [this pattern])
  )

(defprotocol MasterNode
  (add-slave! [this slave]))

(defprotocol ReadWrite
  (write-key!     [this data])
  (read-key       [this data])
  (replicate-key! [this data])
  )

(defrecord Master [rw-strategy name slaves])
(defrecord Slave  [rw-strategy name master])

(defrecord InMemoryReadWrite [store])
(def memory-store (ref {}))

(extend-type InMemoryReadWrite ReadWrite
  (write-key!
    [this data]
   `(alter memory-store conj {(keyword (:key ~data)) (:val ~data)})
    )
  (read-key
    [this keys]
    (-> (select-keys @(:store this) (vec keys))))
  (replicate-key!
    [this data]
    `(alter memory-store conj {(keyword (:key ~data)) (:val ~data)})
    )
  )
;)


(extend-type Master Node
  (write!
    [this data]
    (eval
      (conj
        (map
          (fn [x] (write-key! (:rw-strategy this) x))
          data)
        `dosync)))

  (read!
    [this keys]
    (println keys)
    (read-key (:rw-strategy this) (map keyword keys)))

  (find-in-keys!
    [this pattern]
    (println pattern)
    (select-keys
      @memory-store
      (filter
        (fn [key]
          (re-matches
            (re-pattern pattern)
            (name key)
            ))
        (keys @memory-store)
        )))

  (find-in-values!
    [this pattern]
    (map
      (fn [key]
        (println pattern)
        (re-matches
          (re-pattern (str pattern))
          (str (get key 0) @memory-store)
          )
        )
      @memory-store
      )))


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
    ;(let [store @(:store (:rw-strategy this))]
    ;  (doseq [[data] store]
    ;    (replicate-key! (:rw-strategy slave) data)))
    this))

(extend-type Slave Node
  (write!
    [this data]
    (-> @(:master this)
        (:rw-strategy)
        (write-key! data)))
  (read!
    [this k]
    (-> (:rw-strategy this)
        (read-key (keyword k))))

  (find-in-keys!
    [this pattern]
    ;(-> (:rw-strategy this)
    ;    (match-key! (keyword pattern)))
    )
  (find-in-values!
    [this pattern]
    ;(let [slaves @(:slaves this)]
    ;  (doseq [{:keys [rw-strategy]} slaves]
    ;    (match-value! rw-strategy pattern)))
    )

  )


(defn master-node
  [name]
  (Master. (InMemoryReadWrite. memory-store) name (atom [])))


(defn slave-memory-node
  ([name]
   (Slave. (InMemoryReadWrite. memory-store) name (atom {})))
  ([name master]
   {:pre [(satisfies? MasterNode master)]}
   (Slave. (InMemoryReadWrite. memory-store) name (atom master))))


(def master  (master-node "master"))
;(def memory1 (slave-memory-node "memory1"))
;(add-slave! master memory1)


(defn execute
  [packet]
  (println packet)
  (let
    [
     command (roar.protocol/get-command-type (-> packet :command))
     value   (-> packet :data :value)
     ]
    (cond
      (= command :get) (->> packet :data :data (map :key) (read! master))
      (= command :set) (write! master (get-in packet [:data :data]) )
      (= command :match-key)   ( find-in-keys!    master key )
      (= command :match-value) ( find-in-values!  master key )
      :else nil
      )
)
)

(defn process [cmd]
  (println
    (str
      (execute
        (roar.protocol/parse-frame cmd)))
    ))


;(def bus (bus/event-bus))
;
;(defn handle-msg [byte-msg stream]
;  (println byte-msg)
;  (process byte-msg)
;  )
;
;(defn bus-handler [s info]
;  (do
;    (stream/connect (bus/subscribe bus "msg") s)
;    (stream/consume #(bus/publish! bus "msg" (handle-msg % s)) s)))
;
;(defn start-server
;  []
;  (tcp/start-server bus-handler {:port 8888}))
;
;(defn -main
;  []
;  (roar.tcp.server/start))