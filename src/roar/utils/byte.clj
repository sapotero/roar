(ns roar.utils.byte)

(defn bytes-to-int
  ([bytes]
   (bytes-to-int (byte-array bytes) (count bytes) 0))
  ([bytes len offset]
   (reduce + 0
     (map (fn [i]
      (let [shift (* (- len 1 i) 8)]
        (bit-shift-left (bit-and (nth bytes (+ i offset)) 0x000000FF) shift)))
          (range 0 len)))))

(defn take-key-val-length [packet]
  (let [keylen (->> packet (take 16) bytes-to-int)
        tail (->> packet (drop 16))] [keylen tail]))

(defn take-key-val [[keylen packet]]
  {:key (->> packet (take keylen) byte-array String.) :tail (->> packet (drop keylen))})

(defn recursive-parse
  [packet]
  (loop [conResult []
         packet packet]
    (if (< 0 (count packet))
      (let [result (-> packet take-key-val-length take-key-val)]
        (recur (conj conResult (:key result)) (:tail result)))
      conResult)))

(defn as-array [data]
  (map
    #(hash-map :key (first %) :val (last %))
    (partition 2 (recursive-parse data))))
