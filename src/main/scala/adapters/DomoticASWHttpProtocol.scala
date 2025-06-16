package adapters

import scala.concurrent.Future
import org.apache.pekko
import pekko.actor.typed.ActorSystem
import pekko.http.scaladsl.Http
import pekko.http.scaladsl.Http.ServerBinding
import pekko.http.scaladsl.model._
import pekko.http.scaladsl.server.Directives._
import pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.{*, given}
import pekko.stream.scaladsl.Sink
import spray.json.DefaultJsonProtocol.{*, given}
import spray.json.*
import domoticasw.DomoticASW.*
import domain.RoombaAgent
import domain.Roomba.Event.*
import domain.Roomba.Mode.*

object DomoticASWDeviceHttpInterface:
  import Marshalling.given
  case class BadRequest(message: String)
  case class NotFound(message: String)
  case class ExecuteActionBody(input: Option[String])
  case class RegisterBody(serverPort: Int)

  def badActionIdMessage(action: String) =
    s"Action \"$action\" not found, known actions are [\"start\", \"stop\", \"setMode\"]"
  def badModeMessage(mode: String) =
    s"Unexpected mode \"$mode\", expected values are [\"Silent\", \"Performance\", \"Deep Cleaning\"]"

  def apply(host: String, port: Int, roombaAgent: RoombaAgent)(
      using a: ActorSystem[Any]
  ): Future[ServerBinding] =
    Http()
      .newServerAt(host, port)
      .connectionSource()
      .to {
        Sink foreach: conn =>
          val clientAddress = conn.remoteAddress
          conn.handleWithAsyncHandler:
            concat( // TODO: add other paths
              (path("execute" / Segment) & entity(as[ExecuteActionBody])):
                // TODO: proper error when unmarshalling fails
                (segment, body) =>
                  val event = segment match
                    case "start" => Right(Start)
                    case "stop"  => Right(Stop)
                    case "set-mode" =>
                      body.input match
                        case Some("Silent") => Right(ChangeMode(Silent))
                        case Some("Performance") =>
                          Right(ChangeMode(Performance))
                        case Some("Deep Cleaning") =>
                          Right(ChangeMode(DeepCleaning))
                        case Some(m) => Left(BadRequest(badModeMessage(m)))
                        case None    => Left(BadRequest(badModeMessage("null")))
                    case _ => Left(NotFound(badActionIdMessage(segment)))
                  post:
                    event match
                      case Left(err @ BadRequest(_)) =>
                        complete(StatusCodes.BadRequest, err)
                      case Left(err @ NotFound(_)) =>
                        complete(StatusCodes.NotFound, err)
                      case Right(value) =>
                        roombaAgent.enqueEvent(value)
                        complete(StatusCodes.OK)
              ,
              (path("register") & entity(as[RegisterBody]) & post): body =>
                val host = clientAddress.getHostName()
                val port = body.serverPort
                println(s"$host:$port")
                val serverHttpAdapter =
                  ServerHttpAdapter(host, port)(using a.executionContext)
                roombaAgent.registerToServer(serverHttpAdapter)
                complete(StatusCodes.OK, roombaRegistration(roombaAgent))
            )
      }
      .run()

  def roombaRegistration(a: RoombaAgent) = DeviceRegistration(
    a.roomba.id.toString(),
    a.roomba.name,
    Seq(
      DeviceProperty.WithTypeConstraint(
        "state",
        "State",
        a.roomba.state.toString(),
        TypeConstraints.None(Type.String)
      ),
      DeviceProperty.WithTypeConstraint(
        "battery",
        "Battery",
        a.roomba.battery,
        TypeConstraints.IntRange(0, 100)
      ),
      DeviceProperty.WithSetter(
        "mode",
        "Mode",
        a.roomba.mode.toString(),
        "set-mode"
      ),
      DeviceProperty.WithTypeConstraint(
        "current-room",
        "Current room",
        a.roomba.currentRoom,
        TypeConstraints.None(Type.String)
      ),
      DeviceProperty.WithTypeConstraint(
        "charging-station-room",
        "Charging station room",
        a.roomba.chargingStationRoom,
        TypeConstraints.None(Type.String)
      ),
      DeviceProperty.WithTypeConstraint(
        "rooms",
        "Rooms",
        a.roomba.rooms.mkString(", "),
        TypeConstraints.None(Type.String)
      )
    ),
    Seq(
      DeviceAction(
        "set-mode",
        "Set mode",
        Some("Sets the cleaning mode"),
        TypeConstraints.Enum(Set("Silent", "Performance", "DeepCleaning"))
      ),
      DeviceAction(
        "start",
        "Start",
        Some("Makes the roomba go cleaning"),
        TypeConstraints.None(Type.Void)
      ),
      DeviceAction(
        "stop",
        "Stop",
        Some("Makes the roomba go charging"),
        TypeConstraints.None(Type.Void)
      )
    ),
    Seq()
  )
object Marshalling:
  import DomoticASWDeviceHttpInterface.*
  given RootJsonFormat[BadRequest] = jsonFormat1(BadRequest.apply)
  given RootJsonFormat[NotFound] = jsonFormat1(NotFound.apply)
  given RootJsonFormat[ExecuteActionBody] = jsonFormat1(ExecuteActionBody.apply)
  given RootJsonFormat[RegisterBody] = jsonFormat1(RegisterBody.apply)

  given RootJsonFormat[Color] = jsonFormat3(Color.apply)
  given RootJsonFormat[Type] = new RootJsonFormat {
    def read(json: JsValue): Type =
      val typeOpt = json match
        case JsString(s) => Type.fromString(s)
        case _           => None
      typeOpt.getOrElse:
        val values = Type.values.mkString(", ")
        deserializationError(s"Available values for type are: ${values}")

    def write(obj: Type): JsValue = JsString(obj.toString())
  }

  given RootJsonFormat[ActualTypes] = new RootJsonFormat {
    def read(json: JsValue): ActualTypes = json match
      case JsObject(f) =>
        val colorOpt = for
          r <- f.get("r")
          r <-
            if r.isInstanceOf[JsNumber] then Some(r.asInstanceOf[JsNumber])
            else scala.None
          g <- f.get("g")
          g <-
            if g.isInstanceOf[JsNumber] then Some(g.asInstanceOf[JsNumber])
            else scala.None
          b <- f.get("b")
          b <-
            if b.isInstanceOf[JsNumber] then Some(b.asInstanceOf[JsNumber])
            else scala.None
        yield (Color(r.value.toInt, g.value.toInt, b.value.toInt))
        colorOpt.getOrElse(deserializationError("Expected Color"))
      case JsString(value)                  => value
      case JsNumber(value) if value.isWhole => value.toInt
      case JsNumber(value)                  => value.toDouble
      case JsNull                           => ()
      case JsTrue                           => true
      case JsFalse                          => false
      case _ =>
        deserializationError(
          "Expected one of [\"Color\",\"String\",\"Int\",\"Double\",\"Null\",\"Boolean\",]"
        )

    def write(obj: ActualTypes): JsValue = obj match
      case obj: Unit    => JsNull
      case obj: Color   => summon[JsonWriter[Color]].write(obj)
      case obj: Boolean => summon[JsonWriter[Boolean]].write(obj)
      case obj: Double  => summon[JsonWriter[Double]].write(obj)
      case obj: Int     => summon[JsonWriter[Int]].write(obj)
      case obj: String  => summon[JsonWriter[String]].write(obj)
  }

  import TypeConstraints.*
  given RootJsonFormat[Enum] = jsonFormat1(Enum.apply)
  given RootJsonFormat[IntRange] = jsonFormat2(IntRange.apply)
  given RootJsonFormat[DoubleRange] = jsonFormat2(DoubleRange.apply)
  given RootJsonFormat[None] = jsonFormat1(None.apply)
  given RootJsonFormat[TypeConstraints] = new RootJsonFormat {
    def read(json: JsValue): TypeConstraints =
      json.asJsObject().fields.get("constraint") match
        case scala.None => deserializationError("expected field \"constraint\"")
        case Some(value) =>
          val fields = JsObject(json.asJsObject().fields - "constraint")
          value match
            case JsString("None") => summon[JsonReader[None]].read(fields)
            case JsString("Enum") => summon[JsonReader[Enum]].read(fields)
            case JsString("IntRange") =>
              summon[JsonReader[IntRange]].read(fields)
            case JsString("DoubleRange") =>
              summon[JsonReader[DoubleRange]].read(fields)
            case JsString(_) =>
              deserializationError(
                "One of [\"None\", \"Enum\", \"IntRange\", \"Double\"] expected for field \"constraint\""
              )
            case _ =>
              deserializationError("String expected for field \"constraint\"")

    def write(obj: TypeConstraints): JsValue =
      obj match
        case Enum(values) =>
          JsObject(
            Map(
              ("constraint" -> JsString("Enum")),
              ("values" -> JsArray(values.map(JsString(_)).toSeq*))
            )
          )
        case IntRange(min, max) =>
          JsObject(
            Map(
              ("constraint" -> JsString("IntRange")),
              ("min" -> JsNumber(min)),
              ("max" -> JsNumber(max))
            )
          )
        case DoubleRange(min, max) =>
          JsObject(
            Map(
              ("constraint" -> JsString("DoubleRange")),
              ("min" -> JsNumber(min)),
              ("max" -> JsNumber(max))
            )
          )
        case None(t) =>
          JsObject(
            Map(
              ("constraint" -> JsString("None")),
              ("type" -> JsString(t.toString()))
            )
          )

  }

  import DeviceProperty.*
  given RootJsonFormat[WithSetter] = jsonFormat4(WithSetter.apply)
  given RootJsonFormat[WithTypeConstraint] =
    jsonFormat4(WithTypeConstraint.apply)
  given RootJsonFormat[DeviceProperty] = new RootJsonFormat {
    def read(json: JsValue): DeviceProperty =
      val fields = json.asJsObject.fields
      fields.contains("setterActionId") match
        case true => summon[JsonReader[WithSetter]].read(json)
        case false if fields.contains("typeConstraints") =>
          summon[JsonReader[WithTypeConstraint]].read(json)
        case false =>
          deserializationError(
            "Expected object containing one of these fields: [\"setterActionId\", \"typeConstraints\"]"
          )

    def write(obj: DeviceProperty): JsValue = obj match
      case obj: WithSetter =>
        summon[JsonFormat[WithSetter]].write(obj)
      case obj: WithTypeConstraint =>
        summon[JsonFormat[WithTypeConstraint]].write(obj)
  }

  given RootJsonFormat[DeviceAction] = jsonFormat4(DeviceAction.apply)

  given RootJsonFormat[DeviceRegistration] =
    jsonFormat5(DeviceRegistration.apply)
