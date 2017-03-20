(ns roar.tcp.test
  (:import [java.nio.channels AsynchronousServerSocketChannel
                              AsynchronousSocketChannel CompletionHandler Channels]
           [java.net InetSocketAddress]
           [java.nio.charset Charset]
           [java.nio ByteBuffer])
  (:require [clojure.core.async
             :refer [go >! chan <! >!! <!! go-loop close! map< split mapcat<]]
            [clojure.java.io :refer [reader writer]]))

(def channel (chan 1e6))

(defn listen-ch
  "return a channel which listens on port, values in the channel are scs of
   AsynchronousSocketChannel"
  ([port]
   (listen-ch port channel))
  ([port ch]
   (let [^AsynchronousServerSocketChannel listener
         (-> (AsynchronousServerSocketChannel/open)
             (.bind (InetSocketAddress. port)))
         handler (reify CompletionHandler
                   (completed [this sc _]
                     (go (>! ch sc))
                     (.accept listener nil this)))]
     (.accept listener nil handler)
     ch)))

(defn read-buf!
  [^ByteBuffer buf cnt]
  (when (pos? cnt)
    (let [bytes (byte-array cnt)
          _ (.flip buf)
          _ (.get buf bytes)
          _ (.clear buf)]
      bytes)))

(defn read-ch
  ([asc]
   (read-ch asc (ByteBuffer/allocateDirect 65535) (chan)))
  ([^AsynchronousSocketChannel asc buf ch]
   (.read asc buf nil
          (reify CompletionHandler
            (completed [this cnt _]
              (if-let [bytes (read-buf! buf cnt)]
                (do
                  (go (>! ch bytes))
                  (.read asc buf nil this))
                (do
                  (.close asc))))))
   ch))


(defn write-socket-channel [channel string close?]
  (let [bytes (.getBytes string)
        buf (ByteBuffer/allocateDirect (.length string))]
    (.put buf bytes)
    (.rewind buf)
    (.write channel buf nil
            (reify CompletionHandler
              (completed [this cnt _]
                (println "wrote" cnt))
              (failed [this e _]
                (.close channel)
                (println "! Failed (write):" e (.getMessage e)))))))

(defn simple [port]
  (let [lc (listen-ch port channel)]
    (go-loop [asc (<! lc)]
      (when asc
        (let [rc (read-ch  asc)]
          (go-loop [bs (<! rc)]
            (when bs
              (write-socket-channel asc @(future (roar.core/process (String. bs))) true)
              (recur (<! rc))))))
      (recur (<! lc)))))


(comment
  (split identity)
  first
  (map< println))