package adapters

import org.apache.pekko
import pekko.actor.typed.ActorSystem
import pekko.http.scaladsl.Http
import pekko.http.scaladsl.model._
import pekko.http.scaladsl.server.Directives._
import pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.{*, given}
import spray.json.DefaultJsonProtocol.{*, given}
import spray.json.RootJsonFormat
import spray.json.JsonFormat
import domain.RoombaAgent
import domain.Roomba.Event.*
import domain.Roomba.Mode.*
import org.apache.pekko.http.scaladsl.Http.ServerBinding
import scala.concurrent.Future

object DomoticASWDeviceHttpInterface:
  case class BadRequest(message: String)
  given RootJsonFormat[BadRequest] = jsonFormat1(BadRequest.apply)
  case class NotFound(message: String)
  given RootJsonFormat[NotFound] = jsonFormat1(NotFound.apply)
  case class ExecuteActionBody(input: String)
  given RootJsonFormat[ExecuteActionBody] = jsonFormat1(ExecuteActionBody.apply)

  def badActionIdMessage(action: String) =
    s"Action \"$action\" not found, known actions are [\"start\", \"stop\", \"setMode\"]"
  def badModeMessage(mode: String) =
    s"Unexpected mode \"$mode\", expected values are [\"Silent\", \"Performance\", \"Deep Cleaning\"]"

  def apply(host: String, port: Int, roombaAgent: RoombaAgent)
    (using a: ActorSystem[Any]): Future[ServerBinding] =
    Http()
      .newServerAt("localhost", 8080)
      .bind:
        // TODO: add other paths
        (path("/execute" / Segment) & entity(as[ExecuteActionBody])):
          // TODO: proper error when unmarshalling fails
          (segment, body) =>
            val event = segment match
              case "start" => Right(Start)
              case "stop"  => Right(Stop)
              case "setMode" =>
                body.input match
                  case "Silent"        => Right(ChangeMode(Silent))
                  case "Performance"   => Right(ChangeMode(Performance))
                  case "Deep Cleaning" => Right(ChangeMode(DeepCleaning))
                  case m               => Left(BadRequest(badModeMessage(m)))
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
