(ns roar.protocol)

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