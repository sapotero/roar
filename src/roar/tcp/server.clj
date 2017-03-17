(ns roar.tcp.server
  (:require [clojure.java.io :as io])
  (:import (java.net ServerSocket)))

(def running (atom true))

(defn receive
  [socket]
  (slurp (io/reader socket)))

(defn raw-handler
  [data ]
  (roar.protocol/parse-frame data))

(defn start-server [port handler]
  (with-open [server-sock (ServerSocket. port)]
    (while @running
      (with-open [sock (.accept server-sock)]
        (let [msg-in  (receive sock)
              msg-out (handler msg-in)]
          (send sock msg-out))))))

(defn start
  []
  (start-server 6666 raw-handler))