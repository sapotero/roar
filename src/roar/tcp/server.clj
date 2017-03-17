(ns roar.tcp.server
  (:require [clojure.java.io :as io])
  (:import (java.net ServerSocket)))

(def running (atom true))

(defn to-seq
  [string]
  (map #(bit-and (int %) 0xFF) string))

(defn receive
  [socket]
  (let [bb  (byte-array 65535)
        rr (.read (io/input-stream socket) bb)] (take (- 65535 rr) (seq bb))) )

(defn raw-handler
  [data]
  (println (roar.protocol/parse-frame data))
  (roar.protocol/parse-frame data))

(defn start-server [port handler]
  (with-open [server-sock (ServerSocket. port)]
    (while @running
      (with-open [sock (.accept server-sock)]
        (let [msg-in  (receive sock)
              msg-out (handler msg-in)]
          (send sock msg-out))))))

(defn start
  "Для того чтобы протестить tcp сервер
   в репле выполнить:
    (roar.tcp.server/start)

   а в консоле:
    cat some |nc localhost 6666"
  []
  (start-server 6666 raw-handler))

