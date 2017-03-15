(ns roar.utils.byte)

(defn bytes-to-int [bytes]
  (->>
    bytes
    (map (partial format "%02x"))
    (apply (partial str "0x"))
    read-string))

(defn take-key-val-length [packet]
  (let [keylen (- (bytes-to-int (->> packet (take 16))) 14)
        tail (->> packet (drop keylen))] [keylen tail]))

(defn take-key-val [[keylen packet]]
  {:key (String. (byte-array  (->> packet (take keylen)))) :tail (->> packet (drop keylen))})

(defn recursive-parse
  ([conResult & packet]
    (let [result (take-key-val (take-key-val-length packet))]
      (apply recursive-parse (conj conResult (:key result)) (:tail result))))
  ([conResult]
    (apply hash-map conResult)))

(defn as-array [data]
  (apply recursive-parse [] data))
