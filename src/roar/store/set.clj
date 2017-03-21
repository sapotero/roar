(ns roar.store.set)

(def set-store {})

(defn update!
  [key value]
  (assoc set-store (keyword key) value))

(defn add!
  [key value]
  (update! key value))

(defn find-by-key!
  [key]
  (let [key (keyword key)]
    (conj {key (@set-store key) })))

(defn remove!
  [key]
  (disj set-store {(keyword key) (find-by-key! key)}))


;(map #(add! (str % "_key") (str % "_val")) (range 1000000))

