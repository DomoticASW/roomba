package ports

import domain.Roomba.*
import scala.concurrent.Future

object ServerCommunicationProtocol:
  case class ServerAddress(host: String, port: Int)

  trait ServerCommunicationProtocol:
    def sendCurrentState(
        serverAddress: ServerAddress,
        roomba: Roomba
    ): Future[Unit]
