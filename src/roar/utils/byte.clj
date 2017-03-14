(ns roar.utils.byte)

(defn bytes-to-int [bytes]
  (->>
    bytes
    (map (partial format "%02x"))
    (apply (partial str "0x"))
    read-string))

(defn as-tuple [data]
  (let
    [
     key-length   (- (bytes-to-int (subvec data 0 16)) 14)
     value-length (- (bytes-to-int (subvec data (+ key-length  16) (+ key-length  32))) 14)
     size (- (count data) (+ key-length value-length 32) )
     ]
    (conj
      {
       :key   (String. (byte-array  (subvec data 16 (+ 16 key-length  ))))
       :value (String. (byte-array  (subvec data (+ key-length  32) (+ key-length  value-length 32))))
       :size size
       })
    )
  )


(defn as-array-tuple [data]
  (let
    [
     key-length   (- (bytes-to-int (subvec data 0 16)) 14)
     value-length (- (bytes-to-int (subvec data (+ key-length  16) (+ key-length  32))) 14)
     size (- data (+ key-length value-length 32))
     ]
    (conj
      {
       :key   (String. (byte-array  (subvec data 16 (+ 16 key-length  ))))
       :value (String. (byte-array  (subvec data (+ key-length  32) (+ key-length  value-length 32))))
       :size   size
       })
    )
  )

(def array (atom []))

(defn as-array [data]
  (let
    [item (as-array-tuple data)]
    (if (= (- data (count item )) 0)
      (sync
        nil
        (conj array item)
        (reset! atom "")
        )
      (swap! array conj item))
    (recur (subvec data (item :size) (count data) ))
    ))
