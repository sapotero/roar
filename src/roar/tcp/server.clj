(ns roar.tcp.server
  (:import
    [java.net InetSocketAddress]
    [io.netty.buffer Unpooled]
    [io.netty.channel ChannelHandler ChannelInboundHandlerAdapter ChannelInitializer ChannelInitializer ChannelHandlerContext ChannelFutureListener]
    [io.netty.channel.nio NioEventLoopGroup]
    [io.netty.bootstrap ServerBootstrap]
    [io.netty.buffer ByteBufUtil]
    [io.netty.channel.socket.nio NioServerSocketChannel]

    )
  (:use [clojure.stacktrace])
  (:gen-class))


(defrecord Server [group channel-future])

(defn decode
  [msg]
  (
    (println "decode msg")
    (ByteBufUtil/getBytes msg)
    ))

(def raw-packet (ref ()))

(defn add-to-packet
  [msg]
  (dosync
    (ref-set
      raw-packet
      (merge @raw-packet msg))))

(defn get-full-packet
  []
  (count @raw-packet))

(defn reset-packet
  []
  (dosync
    (ref-set raw-packet [])))

(defn ^ChannelHandler echo-handler []
  (proxy [ChannelInboundHandlerAdapter]
         []
    (channelRead [^ChannelHandlerContext ctx msg]
      (println "Received : " (.capacity msg))
      (add-to-packet msg)
      ;(.writeAndFlush ctx msg)
      (.flush)
      )
    (channelReadComplete [^ChannelHandlerContext ctx]
      (println "Read Complete: " (get-full-packet))
      (reset-packet)
      ;(-> ctx (.writeAndFlush Unpooled/EMPTY_BUFFER))
      )
    (exceptionCaught [^ChannelHandlerContext ctx cause]
      (println "exceptionCaught")
      (println cause)
      (.close ctx))))



(defn close-server [{:keys [group channel-future]}]
  (-> channel-future .channel .closeFuture)
  (.shutdownNow group))

(defn ^ChannelInitializer channel-initializer []
  (proxy [ChannelInitializer]
         []
    (initChannel [ch]
      (-> ch (.pipeline) (.addLast (into-array ChannelHandler [(echo-handler)])))

      )))

;(defn thread-pool
;  []
;  (proxy [NioServerSocketChannelFactory] []))

(defn start-server [port]
  (let [group (NioEventLoopGroup.)
        b (ServerBootstrap.)
        ]
    (-> b (.group group)
        (.channel NioServerSocketChannel)
        (.localAddress (InetSocketAddress. port))
        (.childHandler (channel-initializer))
        )
    (->Server group (-> b .bind .sync))
    ))

(start-server 6325)