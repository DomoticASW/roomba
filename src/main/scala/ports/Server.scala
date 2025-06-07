package ports

import domain.Roomba.*
import scala.concurrent.Future

trait Server {
  def sendCurrentState(roomba: Roomba): Future[Unit]
}
