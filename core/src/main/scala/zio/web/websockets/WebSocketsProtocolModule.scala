package zio.web.websockets

import java.io.IOException

import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.logging.Logging
import zio.web._

trait WebSocketsProtocolModule extends ProtocolModule {
  type ServerConfig       = WebSocketsServerConfig
  type ClientConfig       = WebSocketsClientConfig
  type ServerService      = Any
  type ClientService[_]   = Nothing
  type MiddleWare[-R, +E] = WebSocketsMiddleware[R, E]
  type MinMetadata[+_]    = Any

  override def makeServer[M[+_] <: MinMetadata[_], R <: Has[ServerConfig]: Tag, E, Ids: Tag](
    middleware: Middleware[R, E],
    endpoints: Endpoints[M, Ids],
    handlers: Handlers[M, R, Ids]
  ): ZLayer[R with Blocking with Logging, IOException, Has[ServerService]] = ???

  override def makeDocs[R, M[+_] <: MinMetadata[_]](endpoints: Endpoints[M, _]): ProtocolDocs = ???

  override def makeClient[M[+_] <: MinMetadata[_], Ids: Tag](
    endpoints: Endpoints[M, Ids]
  ): ZLayer[Has[ClientConfig] with Clock with Logging, IOException, Has[ClientService[Ids]]] = ???

}
