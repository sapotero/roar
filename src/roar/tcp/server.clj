(ns roar.tcp.server
  (:import   [io.netty.bootstrap AbstractBootstrap ServerBootstrap]
             [io.netty.channel ChannelFuture ChannelInitializer ChannelOption
                               ChannelHandlerContext ChannelInboundHandlerAdapter ChannelHandler]
             [io.netty.handler.logging LogLevel LoggingHandler]
             io.netty.buffer.ByteBuf
             io.netty.channel.socket.SocketChannel
             io.netty.channel.nio.NioEventLoopGroup
             io.netty.channel.socket.nio.NioServerSocketChannel
             java.util.concurrent.atomic.AtomicInteger)
  (:use [clojure.stacktrace])
  (:gen-class))

;; the actual server code
(def echo-server-handler
  (proxy [ChannelInboundHandlerAdapter] []
    (channelRead [ctx msg]
      (println msg)
      (.write ctx msg))
    (channelReadComplete [ctx]
      (.flush ctx))
    (exceptionCaught [ctx cause]
      (print-stack-trace cause)
      (.close ctx)
      )))

(defn create-tcp-server
  [bossGroup workerGroup]
  (->  (ServerBootstrap.)
       (.group bossGroup workerGroup)
       (.channel io.netty.channel.socket.nio.NioServerSocketChannel)
       (.option  ChannelOption/SO_BACKLOG (int 80))
       ;(.childOption ChannelOption/SO_KEEPALIVE true)
       (.childHandler
         (proxy [ChannelInitializer] []
           (initChannel [ch]
             (-> ch
                 (.pipeline)
                 (.addLast (echo-server-handler))))))
       ))

(defn create-future-channel
  [server]
  (let [future
        (-> server
            (.bind 2323)
            (.sync))]
    (
      (-> future
          (.channel)
          (.closeFuture)
          (.sync))
      )))

(defn start []
  (let [bossGroup   (NioEventLoopGroup.)
        workerGroup (NioEventLoopGroup.)
        server (create-tcp-server bossGroup workerGroup)
        ]
    (create-future-channel server)))