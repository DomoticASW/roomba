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
import domain.RoombaAgent
import domain.Roomba.Event.*
import domain.Roomba.Mode.*

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
}
"""

object DomoticASWDeviceHttpInterface:
  case class BadRequest(message: String)
  given RootJsonFormat[BadRequest] = jsonFormat1(BadRequest.apply)
  case class NotFound(message: String)
  given RootJsonFormat[NotFound] = jsonFormat1(NotFound.apply)
  case class ExecuteActionBody(input: Option[String])
  given RootJsonFormat[ExecuteActionBody] = jsonFormat1(ExecuteActionBody.apply)

  def badActionIdMessage(action: String) =
    s"Action \"$action\" not found, known actions are [\"start\", \"stop\", \"setMode\"]"
  def badModeMessage(mode: String) =
    s"Unexpected mode \"$mode\", expected values are [\"Silent\", \"Performance\", \"Deep Cleaning\"]"

  def apply(host: String, port: Int, roombaAgent: RoombaAgent)
    (using a: ActorSystem[Any]): Future[ServerBinding] =
    Http()
      .newServerAt(host, port)
      .bind:
        // TODO: add other paths
        (path("execute" / Segment) & entity(as[ExecuteActionBody])):
          // TODO: proper error when unmarshalling fails
          (segment, body) =>
            val event = segment match
              case "start" => Right(Start)
              case "stop"  => Right(Stop)
              case "setMode" =>
                body.input match
                  case Some("Silent")        => Right(ChangeMode(Silent))
                  case Some("Performance")   => Right(ChangeMode(Performance))
                  case Some("Deep Cleaning") => Right(ChangeMode(DeepCleaning))
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
