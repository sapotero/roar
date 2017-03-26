(ns roar.tcp.server
  (:import
    [java.net InetSocketAddress]
    [org.jboss.netty.channel ChannelHandler ChannelHandlerContext ChannelFutureListener ChannelPipelineFactory Channels SimpleChannelUpstreamHandler MessageEvent]
    [org.jboss.netty.bootstrap ServerBootstrap]
    [org.jboss.netty.buffer ChannelBuffers]
    [org.jboss.netty.channel.socket.nio NioServerSocketChannel NioServerSocketChannelFactory]

    (java.util.concurrent Executors TimeUnit)
    (org.jboss.netty.handler.execution OrderedMemoryAwareThreadPoolExecutor)
    (org.jboss.netty.handler.codec.string StringDecoder StringEncoder)
    (org.jboss.netty.handler.codec.frame FrameDecoder)
    (java.time LocalDateTime)
    (hash Functions))
  (:use [clojure.stacktrace])
  (:gen-class))

(def store (atom {}))
(defn add-to-store
  []
  (let [time (str (LocalDateTime/now))]
    (swap! store conj {(keyword (Long/toUnsignedString (hash.Functions/DJBHash time))) time})))

(defrecord Server [#^ServerBootstrap bootstrap channel])

(defn response
  [status]
  (ChannelBuffers/copiedBuffer (str "Success: " status) "UTF-8"))

(defn make-handler
  []
  (proxy [SimpleChannelUpstreamHandler] []
    (messageReceived [ctx e]
      (let [c  (.getChannel e)
            cb (.getMessage e)
            ]
        (future (add-to-store))
        (.write c (response cb))
        ;(-> e .getChannel .close)
        ))

    (exceptionCaught
      [ctx e]
      (let [throwable (.getCause e)]
        ;убийца производительности
        ;(println "@exceptionCaught" throwable)
        )
      (-> e .getChannel .close))))

(defn start-server [port]
  (let [
        boss (OrderedMemoryAwareThreadPoolExecutor. 1 (* 128 1024 1024) (* 256 1024 1024) 60 TimeUnit/SECONDS)
        work (OrderedMemoryAwareThreadPoolExecutor. 4 (* 32 1024 1024) (* 128  1024 1024) 60 TimeUnit/SECONDS)
        bootstrap (ServerBootstrap. (NioServerSocketChannelFactory. boss work 4))
        pipeline  (.getPipeline bootstrap)
        ]
    (.addLast pipeline "decoder" (StringEncoder.))
    (.addLast pipeline "encoder" (StringEncoder.))
    (.addLast pipeline "handler" (make-handler))

    ;(.setOption bootstrap "child.keepAlive"  true)
    (.setOption bootstrap "child.tcpNoDelay" true)
    (.setOption bootstrap "child.sendBufferSize"    (* 512 1024))
    (.setOption bootstrap "child.receiveBufferSize" (* 512 1024))
    (.setOption bootstrap "backlog" (* 8 16))

    (Server. bootstrap (.bind bootstrap (InetSocketAddress. port))))
    )

(start-server 6325)
;iperf --filename some -c 127.0.0.1 -p 6325 -t 10 -o iperf.result -P 32