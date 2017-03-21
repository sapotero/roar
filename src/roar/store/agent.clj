(ns roar.store.agent)

(def agent-store (agent {}))

(defn update!
  [key value]
  (send agent-store conj {(keyword key) value}))

(defn add!
  [key value]
  (future (update! key value)))

(defn find-by-key!
  [key]
  (let [key (keyword key)]
    (conj {key (@agent-store key) })))

(defn remove!
  [key]
  (send agent-store disj {(keyword key) (find-by-key! key)}))


(map #(add! (str % "_key") (str % "_val")) (range 1000000))

