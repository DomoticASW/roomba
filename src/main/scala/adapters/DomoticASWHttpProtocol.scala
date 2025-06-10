package adapters

import scala.concurrent.Future
import org.apache.pekko
import pekko.actor.typed.ActorSystem
import pekko.http.scaladsl.Http
import pekko.http.scaladsl.Http.ServerBinding
import pekko.http.scaladsl.model._
import pekko.http.scaladsl.server.Directives._
import pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.{*, given}
import spray.json.DefaultJsonProtocol.{*, given}
import spray.json.*
import domoticasw.DomoticASW.*
import domain.RoombaAgent
import domain.Roomba.Event.*
import domain.Roomba.Mode.*
import java.util.UUID

object DomoticASWDeviceHttpInterface:
  import Marshalling.given
  case class BadRequest(message: String)
  case class NotFound(message: String)
  case class ExecuteActionBody(input: Option[String])

  def roombaRegistration(a: RoombaAgent) = DeviceRegistration(
    UUID.randomUUID().toString(),
    a.roomba.name,
    Seq(),
    Seq(),
    Seq()
  )

  def badActionIdMessage(action: String) =
    s"Action \"$action\" not found, known actions are [\"start\", \"stop\", \"setMode\"]"
  def badModeMessage(mode: String) =
    s"Unexpected mode \"$mode\", expected values are [\"Silent\", \"Performance\", \"Deep Cleaning\"]"

  def apply(host: String, port: Int, roombaAgent: RoombaAgent)(
      using a: ActorSystem[Any]
  ): Future[ServerBinding] =
    Http()
      .newServerAt(host, port)
      .bind:
        concat( // TODO: add other paths
          (path("execute" / Segment) & entity(as[ExecuteActionBody])):
            // TODO: proper error when unmarshalling fails
            (segment, body) =>
              val event = segment match
                case "start" => Right(Start)
                case "stop"  => Right(Stop)
                case "setMode" =>
                  body.input match
                    case Some("Silent")      => Right(ChangeMode(Silent))
                    case Some("Performance") => Right(ChangeMode(Performance))
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
          (path("register") & post):
            complete(StatusCodes.OK, roombaRegistration(roombaAgent))
        )

object Marshalling:
  val registerJSONExample = """
  {
    "id": "128012392139102821"
    "name": "Roomba"
    "properties": [
      {
        "id": "battery",
        "name": "Battery",
        "value": 50,
        "typeConstraints": {
          "constraint": "IntRange",
          "min": 0,
          "max": 100
        }
      },
      {
        "id": "state",
        "name": "State",
        "value": "Cleaning",
        "typeConstraints": {
          "constraint": "Enum",
          "values": ["Charging", "Cleaning", "Going charging"]
        }
      },
      {
        "id": "mode",
        "name": "Mode",
        "value": "Silent",
        "setterActionId": "setMode"
      },
      {
        "id": "currentRoom",
        "name": "Current room",
        "value": "Bathroom",
        "typeConstraints": {
          "type": "String",
          "constraint": None
        }
      },
    ]
    "actions": [
      {
        "id": "start",
        "name": "Start",
        "description": "The roomba will start cleaning",
        "inputTypeConstraints": {
          "type": "Void",
          "constraint": "None"
        }
      },
      {
        "id": "stop",
        "name": "Stop",
        "description": "The roomba will stop cleaning and return to its charging station",
        "inputTypeConstraints": {
          "type": "Void",
          "constraint": "None"
        }
      },
      {
        "id": "setMode",
        "name": "Set mode",
        "description": null,
        "inputTypeConstraints": {
          "constraint": "Enum",
          "values": ["Silent", "DeepCleaning", "Performance"]
        }
      }
    ]
    "events": ["started", "stopped", "low-battery"]
  }"""
  import DomoticASWDeviceHttpInterface.*
  given RootJsonFormat[BadRequest] = jsonFormat1(BadRequest.apply)
  given RootJsonFormat[NotFound] = jsonFormat1(NotFound.apply)
  given RootJsonFormat[ExecuteActionBody] = jsonFormat1(ExecuteActionBody.apply)

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
