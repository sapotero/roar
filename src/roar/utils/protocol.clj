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
  (println (class string))
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
     type (byte/bytes-to-int (subvec seq 0 1))
     ]
    (conj {
        :type type
        :data (parse-packet-data (drop 1 seq) (get-data-type type))
       })))

(defn parse
  ([result data c & d]
    (let [ val (take data d) ]
      (apply
        parse (conj result val) (drop d data) d)))
  ([result data c]
   (conj
     result
     (byte/bytes-to-int (take c data))
     (drop c data))))

(defn parse-frame [data]
  (let
    [
     package (to-int-seq data)
     id      (byte/bytes-to-int (subvec package 0 2))
     command (byte/bytes-to-int (subvec package 2 3))
     length  (byte/bytes-to-int (subvec package 3 35))
     data    (parse-data (subvec package 35 (+ 35 length)))
     ]
    {
     :id      id
     :command command
     :length  length
     :data    data
     }))