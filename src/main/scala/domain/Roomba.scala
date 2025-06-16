package domain

import scala.util.Random
import fsm.FSM.*
import Utils.*

object Roomba extends RoombaOps:
  export RoombaFSM.State
  export RoombaFSM.Mode
  export RoombaFSM.Event

  opaque type Roomba = FSM[State, RoombaFSM.RoombaData, Event]

  object Roomba:
    def apply(
        id: String,
        initialState: State,
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
        roombaData = RoombaFSM.RoombaData(
          id,
          name,
          battery,
          mode,
          currentRoom,
          chargingStationRoom,
          rooms,
          batteryRateMs,
          changeRoomRateMs
        )
      yield (FSM(initialState, roombaData))

  case class BadConfiguration(message: String)

  import fsm.FSM.step as fsmStep
  import fsm.FSM.state as fsmState
  extension (fsm: Roomba)
    def id: String = fsm.data.id
    def state: State = fsm.fsmState
    def name: String = fsm.data.name
    def battery: Int = fsm.data.battery
    def mode: Mode = fsm.data.mode
    def currentRoom: String = fsm.data.currentRoom
    def chargingStationRoom: String = fsm.data.chargingStationRoom
    def rooms: Set[String] = fsm.data.rooms
    def batteryRateMs: Long = fsm.data.batteryRateMs
    def changeRoomRateMs: Long = fsm.data.changeRoomRateMs
    def step(ms: Long, e: Option[Event]): Roomba = fsm.fsmStep(ms, e)

trait RoombaOps:
  import Roomba.*
  extension (r: Roomba)
    def id: String
    def name: String
    def state: State
    def battery: Int
    def mode: Mode
    def currentRoom: String
    def chargingStationRoom: String
    def rooms: Set[String]
    def step(ms: Long, e: Option[Event]): Roomba
