(ns roar.utils.protocol)
(require '[roar.utils.byte :as byte])


(defn get-command-type [code]
  (cond
    (= code 40) :set
    (= code 20) :get
    (= code 31) :match-key
    (= code 32) :match-value
    :else nil
    ))

(defn get-data-type [code]
  (cond
    (= code 1) :tuple
    (= code 2) :array
    :else nil
    ))

(defn to-int-seq
  [string]
  (map #(byte (int %)) string))

(defn to-seq
  [string]
  (map #(bit-and (int %) 0xFF) string))

(defn to-vec
  [string]
  (vec (map #(cond (> 0 %) (bit-and % 0xFF) true (int %)) string)))

(defn parse-packet-data
  [data type]
  (byte/as-array data))

(defn parse-data
  [seq]
  (let
    [
     type (byte/bytes-to-int (take 1 seq))
     ]
    (conj {
           :type type
           :data (parse-packet-data (drop 1 seq) (get-data-type type))
           })))

(defn parse
  ([result data c & d]
   (let [val (take c data)] (apply parse (conj result val) (drop c data) d)))
  ([result data c]
   (conj result (take c data) (drop c data))))

(defn parse-frame [data]
  (let [raw-result (apply parse [] (to-seq data) '(2 1 32))]
    {
     :id (String. (byte-array (nth raw-result 0)))
     :command (roar.utils.byte/bytes-to-int (nth raw-result 1))
     :length (roar.utils.byte/bytes-to-int (nth raw-result 2))
     :data (parse-data (nth raw-result 3))
     }))