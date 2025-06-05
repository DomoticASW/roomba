package domain

import Utils.*

object Roomba extends RoombaOps:

  opaque type Roomba = RoombaImpl
  private case class RoombaImpl(
      name: String,
      battery: Int,
      mode: Mode,
      currentRoom: String,
      chargingStationRoom: String,
      rooms: Set[String],
      batteryRateMs: Long,
      changeRoomRateMs: Long
  )

  enum State:
    case Cleaning
    case GoingCharging
    case Charging

  enum Mode:
    case Silent
    case Performance
    case DeepCleaning

  object Roomba:
    def apply(
        name: String,
        battery: Int,
        mode: Mode,
        currentRoom: String,
        chargingStationRoom: String,
        rooms: Set[String],
        batteryRateMs: Long,
        changeRoomRateMs: Long
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
        _ <- Either.leftIf(
          batteryRateMs > 0 && changeRoomRateMs > 0,
          BadConfiguration("Rates must be higher than 0 milliseconds")
        )
      yield (RoombaImpl(
        name,
        battery,
        mode,
        currentRoom,
        chargingStationRoom,
        rooms,
        batteryRateMs,
        changeRoomRateMs
      ))

  case class BadConfiguration(message: String)

  extension (r: Roomba)
    def name: String = r.name
    def battery: Int = r.battery
    def mode: Mode = r.mode
    def currentRoom: String = r.currentRoom
    def chargingStationRoom: String = r.chargingStationRoom
    def rooms: Set[String] = r.rooms
    def batteryRateMs: Long = r.batteryRateMs
    def changeRoomRateMs: Long = r.changeRoomRateMs
    def update(
        battery: Int,
        mode: Mode,
        currentRoom: String
    ): Roomba =
      r.copy(
        battery = battery,
        mode = mode,
        currentRoom = currentRoom
      )

trait RoombaOps:
  import Roomba.*
  extension (r: Roomba)
    def name: String
    def battery: Int
    def mode: Mode
    def currentRoom: String
    def chargingStationRoom: String
    def rooms: Set[String]
    def update(
        battery: Int = r.battery,
        mode: Mode = r.mode,
        currentRoom: String = r.currentRoom
    ): Roomba
