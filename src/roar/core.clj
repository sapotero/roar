(ns roar.core  (:gen-class) )
(require '[roar.protocol :as proto])

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
  (match-key!     [this pattern])
  (match-value!   [this pattern])
  )

(defrecord Master [rw-strategy name slaves])
(defrecord Slave  [rw-strategy name master])

(defrecord InMemoryReadWrite [store])

(extend-type InMemoryReadWrite ReadWrite
  (write-key!
    [this array]
    (doall
      (map (fn [x] (swap! (:store this) conj {(keyword (:key x)) (x :val)})) array)))
  (read-key
    [this keys]
    (-> (select-keys @(:store this) (vec keys))))
  (replicate-key!
    [this array]
    (println (str "array: " array))
    (doall
      (map (fn [x] (swap! (:store this) conj {(keyword (:key x)) (x :val)})) array)))
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
    [this data]
    (-> (:rw-strategy this)
        (write-key! data))
    ;(let [slaves @(:slaves this)]
    ;  (doseq [{:keys [rw-strategy]} slaves]
    ;    (replicate-key! rw-strategy data)))
    this)
  (read!
    [this keys]
    (println keys)
    (read-key (:rw-strategy this) (map keyword keys)))
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
      (doseq [[data] store]
        (replicate-key! (:rw-strategy slave) data)))
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


(def master  (master-node "master"))
(write! master '({:key "some", :val "other"}))
(println (read! master '(:some)))
;(def memory1 (slave-memory-node "memory1"))
;(def memory2 (roar.core/slave-memory-node "memory2"))
;
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
