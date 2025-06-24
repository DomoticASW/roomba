package adapters

import scala.concurrent.*
import sttp.model.*
import sttp.client4.quick.*
import sttp.client4.DefaultFutureBackend
import upickle.default.*
import ports.ServerCommunicationProtocol.*
import domain.Roomba.*
import domoticasw.DomoticASW
import domoticasw.DomoticASW.ActualTypes
import upickle.core.Visitor
import domoticasw.DomoticASW.Color

class ServerCommunicationProtocolHttpAdapter(
    private val serverPortToWhichAnnounce: Int,
    private val clientPortToAnnounce: Int
)(
    using ExecutionContext
) extends ServerCommunicationProtocol:
  case class RoombaState(
      state: State,
      battery: Int,
      mode: Mode,
      currentRoom: String
  )

  given Writer[Color] = Writer.derived
  given Writer[DomoticASW.ActualTypes] with
    def write0[V](out: Visitor[?, V], v: ActualTypes): V =
      v match
        case obj: Unit   => out.visitNull(-1)
        case obj: Color  => summon[Writer[Color]].write0(out, obj)
        case true        => out.visitTrue(-1)
        case false       => out.visitFalse(-1)
        case obj: Double => out.visitFloat64(obj, -1)
        case obj: Int    => out.visitInt32(obj, -1)
        case obj: String => out.visitString(obj, -1)

  case class UpdatePropertyItem(
      propertyId: String,
      value: DomoticASW.ActualTypes
  ) derives Writer
  var prevState: Option[RoombaState] = None
  override def sendCurrentState(
      address: ServerAddress,
      roomba: Roomba
  ): Future[Unit] =
    val currentState = RoombaState(
      roomba.state,
      roomba.battery,
      roomba.mode,
      roomba.currentRoom
    )
    prevState match
      case Some(prevState) if prevState == currentState => Future(())
      case _ =>
        prevState = Some(currentState)
        val updates = Seq(
          UpdatePropertyItem("state", currentState.state.toString()),
          UpdatePropertyItem("battery", currentState.battery),
          UpdatePropertyItem("mode", currentState.mode.toString()),
          UpdatePropertyItem("current-room", currentState.currentRoom)
        )
        quickRequest
          .httpVersion(HttpVersion.HTTP_1_1)
          .patch(
            uri"http://${address.host}:${address.port}/api/devices/${roomba.id}/properties"
          )
          .contentType(MediaType.ApplicationJson)
          .body(write(updates))
          .send(DefaultFutureBackend())
          .recoverWith(err =>
            Console.err.println(err)
            Future.failed(err)
          )
          .map(_ => ())

  import java.net.{DatagramPacket, DatagramSocket, InetAddress}
  import java.nio.charset.StandardCharsets
  import scala.util.Using

  case class AnnounceMessage(id: String, name: String, port: Int) derives Writer

  override def announce(roomba: Roomba): Unit =
    Using(DatagramSocket()): socket =>
      socket.setBroadcast(true)
      val data =
        write(AnnounceMessage(roomba.id, roomba.name, clientPortToAnnounce))
          .getBytes(StandardCharsets.UTF_8)
      // Change to your local subnet broadcast address if needed, e.g., "192.168.1.255"
      val broadcastAddress = InetAddress.getByName("255.255.255.255")
      val packet = new DatagramPacket(
        data,
        data.length,
        broadcastAddress,
        serverPortToWhichAnnounce
      )
      socket.send(packet)
