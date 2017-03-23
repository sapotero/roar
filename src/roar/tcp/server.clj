(ns roar.tcp.server
  (:import
    [java.net InetSocketAddress]
    [io.netty.buffer Unpooled]
    [io.netty.channel ChannelHandler ChannelInboundHandlerAdapter ChannelInitializer ChannelInitializer ChannelHandlerContext ChannelFutureListener]
    [io.netty.channel.nio NioEventLoopGroup]
    [io.netty.bootstrap ServerBootstrap]
    [io.netty.buffer ByteBufUtil]
    [io.netty.channel.socket.nio NioServerSocketChannel])
  (:use [clojure.stacktrace])
  (:gen-class))


(defrecord Server [group channel-future])

(defn decode [msg] (vec (ByteBufUtil/getBytes msg)))

(defn ^ChannelHandler echo-handler []
  (proxy [ChannelInboundHandlerAdapter]
         []
    (channelRead [^ChannelHandlerContext ctx msg]

      (prn "Received : " (roar.core/process (decode msg)))
      (.writeAndFlush ctx msg))
    (channelReadComplete [^ChannelHandlerContext ctx]
      ;(-> ctx (.writeAndFlush Unpooled/EMPTY_BUFFER)
      ; (.addListener ChannelFutureListener/CLOSE))
      )
    (exceptionCaught [^ChannelHandlerContext ctx cause]
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