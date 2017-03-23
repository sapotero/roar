(ns roar.aleph.tcp
  (:require
    [aleph.tcp :as tcp]
    [gloss.core :as gloss]
    [manifold.stream :as s]))

(defn fast-echo-handler
  [f]
  (fn [s info]
    (s/connect
      (s/stream->seq (roar.core/process s))
      s)))


(tcp/start-server
  (fast-echo-handler str)
  {:port 9000, :frame (gloss/string :utf-8 :delimiters ["***\n"])})