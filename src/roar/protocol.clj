(ns roar.protocol (:import (java.nio ByteBuffer ByteOrder)))
(require 'clojure.pprint)

(defn stringToByte
  [string]
  (map (comp byte int) string))

(defn byteToBitString
  [byteList]
  (map #(clojure.pprint/cl-format nil "~8,'0',B" %)
       byteList))

(defn getCommandType [header]
  (let [code (.codePointAt (str header) 0)]
    (cond
      (= code 001) :set
      (= code 002) :get
      (= code 003) :match-key
      (= code 004) :match-value
      :else nil
    )))

(defn- ^BigInteger bytes2int
  [^bytes bytes & {:keys [little-endian]
                   :or {little-endian true}}]
  (let [b (if little-endian bytes  (reverse bytes))]
    (->> b
         (cons (byte 0))
         (byte-array)
         (biginteger))))

(defn getLength
  [command]
  (bytes2int (bytes (byte-array (map (comp byte int) (.substring command 1 3)))))
  )

(defn getData [z length]
  ; проверка
  (.substring z 3 (count z))
  )

(defn- with-native-order [^ByteBuffer buf]
  (.order buf (ByteOrder/LITTLE_ENDIAN)))

(defn buffer
  [^long size]
  (with-native-order (ByteBuffer/allocate size)))

(defn decode
  [^ByteBuffer buffer]
  (let
    [
     command (.get      (with-native-order buffer)  )
     length  (.getShort (with-native-order buffer) 1)
     nb      (bytes (byte-array length))
     ;data    (.get buffer)
     data    (.get (with-native-order buffer) nb 1 3)
     ]
    (set {
          :command command,
          :length length,
          :data data,
          :nb nb
          })))
(defn testa
  []
  (conj
    {}
    (decode
      (ByteBuffer/wrap
              (bytes
                (byte-array
                  (map
                    (comp byte int) "123456"
                    )))))
    ))