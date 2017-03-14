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




(defn bytes-to-int [bytes]
  (->>
    bytes
    (map (partial format "%02x"))
    (apply (partial str "0x"))
    read-string))


(defn test-buffer
  [string]
  (vec (byte-array (map (comp int int) string))))



(defn parse-data
  [string]
  (let
    [
     package      (test-buffer string)
     type         (bytes-to-int (subvec package 0 1))
     key-length   (bytes-to-int (subvec package 1 17))
     key          (String. (byte-array (subvec package 17 (+ 17 key-length))))
     value-length (bytes-to-int (subvec package (+ key-length 17) (+ key-length 17 16)))
     value        (String. (byte-array (subvec package (+ key-length 17 16) (+ key-length value-length 17 16))))
     ]
    (conj {} {
        :type         type
        :key-length   key-length
        :key          key
        :value-length value-length
        :value        value
       })))

(defn generate-packet
  []
  (byte-array
    [
     (int 106)
     (int 81)
     (int 1)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 41)
     (int 1)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 3)
     (int 107)
     (int 101)
     (int 121)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 0)
     (int 5)
     (int 118)
     (int 97)
     (int 108)
     (int 117)
     (int 101)
     (int 10)
     ]))

(defn parse-frame
  [string]
  {:pre (>= (count string) 40)}
  (let
    [
     package (test-buffer string)
     id      (bytes-to-int (subvec package 0 2))
     command (bytes-to-int (subvec package 2 3))
     length  (bytes-to-int (subvec package 3 35))
     data    (->> (parse-data (subvec package 35 (count package))))
     ]
    (println (conj {}  {
       :id      id
       :command command
       :length  length
       :data    data
       }))))

(defn parse-raw-frame
  [byte-array]
  (let
    [
     package (vec byte-array)
     id      (bytes-to-int (subvec package 0 2))
     command (bytes-to-int (subvec package 2 3))
     length  (bytes-to-int (subvec package 3 35))
     ;data    (subvec package 35 (+ 35 length))
     data    (->> (parse-data (subvec package 35 (+ 35 length))))
     ]
    (println
      (conj
        {}
        {
          :id      id
          :command command
          :length  length
          :data    data
          }))))
