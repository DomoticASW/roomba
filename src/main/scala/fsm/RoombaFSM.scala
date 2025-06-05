package fsm

import state.State.State

object RoombaFSM:
  import scala.util.Random
  import domain.Roomba.{State as RoombaState, *}
  import RoombaState.*

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop

  import FSM.*

  type RoombaFSM = FSM[RoombaState, Roomba, Event]

  val batteryRate = 1000
  val changeRoomRate = 4000

  private def setMode(mode: Mode): State[RoombaFSM, Unit] =
    FSM.modified((_: Roomba).update(mode = mode))
  private def incBattery(): State[RoombaFSM, Unit] =
    FSM.modified((r: Roomba) => r.update(battery = r.battery + 1))
  private def decBattery(): State[RoombaFSM, Unit] =
    FSM.modified((r: Roomba) => r.update(battery = r.battery - 1))
  private def changeRoomRandom(): State[RoombaFSM, Unit] =
    FSM.modified((r: Roomba) =>
      r.update(currentRoom = Random().shuffle(r.rooms).head)
    )

  given States[RoombaState, Roomba, Event] with
    override def onEntry(): State[RoombaFSM, Unit] =
      FSM.matchCurrentState:
        (_: RoombaState) match
          case Cleaning =>
            for
              _ <- FSM.setCountdown("battery", batteryRate)
              _ <- FSM.setCountdown("changeRoom", changeRoomRate)
            yield ()
          case GoingCharging =>
            FSM.setCountdown("changeRoom", changeRoomRate)
          case Charging => FSM.setCountdown("battery", batteryRate)

    override def onActive(
        e: Option[Event],
        timePassed: Long
    ): State[RoombaFSM, RoombaState] =
      for
        _ <- e match
          case Some(Event.ChangeMode(mode)) => setMode(mode)
          case _                            => State.same
        nextState <- FSM.matchCurrentState:
          (_: RoombaState) match
            case Cleaning =>
              for
                _ <- FSM.ifCountdownReached("battery")(decBattery())
                _ <- FSM.ifCountdownReached("changeRoom")(changeRoomRandom())
                _ <- FSM.resetCountdownIfReached("battery")
                _ <- FSM.resetCountdownIfReached("changeRoom")
                battery <- FSM.inspect((_: Roomba).battery)
                nextState =
                  if battery <= 10 || e == Some(Event.Stop) then GoingCharging
                  else Cleaning
              yield (nextState)
            case Charging =>
              for
                _ <- FSM.ifCountdownReached("battery")(incBattery())
                _ <- FSM.resetCountdownIfReached("battery")
                nextState =
                  if e == Some(Event.Start) then Cleaning
                  else Charging
              yield (nextState)
            case GoingCharging =>
              for
                _ <- FSM.ifCountdownReached("changeRoom")(changeRoomRandom())
                _ <- FSM.resetCountdownIfReached("changeRoom")
                newRoom <- FSM.inspect((_: Roomba).currentRoom)
                chargingRoom <- FSM.inspect((_: Roomba).chargingStationRoom)
                nextState =
                  if e == Some(Event.Start) then Cleaning
                  else if newRoom == chargingRoom then Charging
                  else GoingCharging
              yield (nextState)
      yield (nextState)
