(ns roar.utils.byte)

(defn bytes-to-int [bytes]
  (->>
    bytes
    (map (partial format "%02x"))
    (apply (partial str "0x"))
    read-string))

(defn take-key-val-length [packet]
  (println packet)
  (let [keylen (- (->> packet (take 16) bytes-to-int) 14)
        tail (->> packet (drop keylen))] (println keylen) [keylen tail]))

(defn take-key-val [[keylen packet]]
  {:key (->> packet (take keylen) byte-array String.) :tail (->> packet (drop keylen))})

(defn recursive-parse
  ([conResult & packet]
    (let [result (-> packet take-key-val-length take-key-val)]
      (apply recursive-parse (conj conResult (:key result)) (:tail result))))
  ([conResult]
    conResult))

(defn as-array [data]
  (map
    #(hash-map :key (first %) :val (last %))
    (partition 2 (apply recursive-parse [] data))))
