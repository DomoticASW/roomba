package domain

import Utils.*

object Roomba extends RoombaOps:

  opaque type Roomba = RoombaImpl
  private case class RoombaImpl(
      name: String,
      battery: Int,
      state: State,
      mode: Mode,
      currentRoom: String,
      chargingStationRoom: String,
      rooms: Set[String]
  )

  enum State:
    case Cleaning
    case Charging

  enum Mode:
    case Silent
    case Performance
    case DeepCleaning

  def apply(
      name: String,
      battery: Int,
      state: State,
      mode: Mode,
      currentRoom: String,
      chargingStationRoom: String,
      rooms: Set[String]
  ): Either[BadConfiguration, Roomba] =
    for
      _ <- Either.leftIf(
        rooms(currentRoom) && rooms(chargingStationRoom),
        BadConfiguration(
          "Given current room and charging station room must be included in given rooms set"
        )
      )
      _ <- Either.leftIf(
        battery >= 0 && battery <= 100,
        BadConfiguration("Given battery should be between 0 and 100")
      )
    yield (RoombaImpl(
      name,
      battery,
      state,
      mode,
      currentRoom,
      chargingStationRoom,
      rooms
    ))

  case class BadConfiguration(message: String)

  extension (r: Roomba)
    def name: String = r.name
    def battery: Int = r.battery
    def state: State = r.state
    def mode: Mode = r.mode
    def currentRoom: String = r.currentRoom
    def chargingStationRoom: String = r.chargingStationRoom
    def rooms: Set[String] = r.rooms

trait RoombaOps:
  extension (r: Roomba.Roomba)
    def name: String
    def battery: Int
    def state: Roomba.State
    def mode: Roomba.Mode
    def currentRoom: String
    def chargingStationRoom: String
    def rooms: Set[String]
