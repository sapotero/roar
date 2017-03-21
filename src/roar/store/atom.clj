(ns roar.store.atom)

(def atom-store (atom {}))

(defn update!
  [key value]
  (swap! atom-store conj {(keyword key) value}))

(defn add!
  [key value]
  @(future (update! key value)))

(defn find-by-key!
  [key]
  (let [key (keyword key)]
    (conj {key (@atom-store key) })))

(defn remove!
  [key]
  (swap! atom-store disj (keyword key)))


;(map #(add! (str % "_key") (str % "_val")) (range 1000000))

