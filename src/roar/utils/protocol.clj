(ns roar.protocol)
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
  (vec (map #(int %) string)))

(defn to-seq
  [string]
  (map #(cond (> 0 %) (bit-and % 0xFF) true (int %)) string))

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
   (let [val (take data d)]) (apply parse (conj result val) (drop d data) d))
  ([result data c] (conj result (byte/bytes-to-int (take c data)) (drop c data))))

(defn parse-frame [data]
  (let [raw-result (parse [] (to-int-seq data) '(2 1 32))]
    {
     :id (subvec raw-result 0)
     :command (subvec raw-result 1)
     :length (subvec raw-result 2)
     :data (parse-data (subvec raw-result 3))
     }))