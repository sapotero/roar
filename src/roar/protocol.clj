(ns roar.protocol (:import (java.nio ByteBuffer ByteOrder)))
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


(defn to-vec
  [string]
  (vec
    (map #(int %) string)))

(defn parse-packet-data
  [data type]
  (byte/as-array data))

(defn parse-data
  [string]
  (let
    [
     package (to-vec string)
     type (byte/bytes-to-int (subvec package 0 1))
     ]
    (conj {
        :type type
        :data (parse-packet-data (subvec package 1 (count package)) (get-data-type type))
       })))

(defn parse-frame
  [string]
  {:pre (>= (count string) 15)}
  (let
    [
     package (to-vec string)
     id      (byte/bytes-to-int (subvec package 0 2))
     command (byte/bytes-to-int (subvec package 2 3))
     length  (byte/bytes-to-int (subvec package 3 35))
     ]
    (println id)
    (println command)
    (println length)
    (conj
      {
       :id      id
       :command command
       :length  length
       :data     (parse-data (subvec package 35 (+ 35 length)))
       })
    ))
;)
