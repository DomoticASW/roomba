package fsm

import state.State.*

object RoombaFSM:
  import scala.util.Random
  import domain.Roomba.{State as RoombaState, *}
  import RoombaState.*

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop

  import FSM.*

  val batteryRate = 1000
  val changeRoomRate = 4000

  private def setMode(mode: Mode): State[Roomba, Unit] =
    State.modify((m: Roomba) => m.update(mode = mode))
  private def incBattery(): State[Roomba, Unit] =
    State.modify((m: Roomba) => m.update(battery = m.battery + 1))
  private def decBattery(): State[Roomba, Unit] =
    State.modify((m: Roomba) => m.update(battery = m.battery - 1))
  private def changeRoomRandom(): State[Roomba, Unit] =
    State.modify((m: Roomba) =>
      m.update(currentRoom = Random().shuffle(m.rooms).head)
    )

  given States[RoombaState, Roomba, Event] with
    def onEntry(s: RoombaState): State[Roomba, Unit] =
      s match
        case Cleaning =>
          for
            _ <- FSM.setCountdown("battery", batteryRate)
            _ <- FSM.setCountdown("changeRoom", changeRoomRate)
          yield ()
        case GoingCharging => FSM.setCountdown("changeRoom", changeRoomRate)
        case Charging      => FSM.setCountdown("battery", batteryRate)

    def onExit(s: RoombaState): State[Roomba, Unit] = State.same
    def onActive(
        s: RoombaState,
        e: Option[Event],
        timePassed: Long
    ): State[Roomba, RoombaState] =
      val handleChangeModeEvent =
        for _ <- e match
            case Some(Event.ChangeMode(mode)) => setMode(mode)
            case _                            => State.same
        yield ()
      val handleActivities = s match
        case Cleaning =>
          for
            updateBattery <- FSM.resetCountdownIfReached("battery")
            changeRoom <- FSM.resetCountdownIfReached("changeRoom")
            _ <- if updateBattery then decBattery() else State.same
            _ <- if changeRoom then changeRoomRandom() else State.same
            battery <- State.inspect((_: Roomba).battery)
            nextState =
              if battery <= 10 || e == Some(Event.Stop) then GoingCharging
              else Cleaning
          yield (nextState)
        case Charging =>
          for
            updateBattery <- FSM.resetCountdownIfReached("battery")
            _ <- if updateBattery then incBattery() else State.same
            nextState =
              if e == Some(Event.Start) then Cleaning
              else Charging
          yield (nextState)
        case GoingCharging =>
          for
            changeRoom <- FSM.resetCountdownIfReached("changeRoom")
            _ <- if changeRoom then changeRoomRandom() else State.same
            newRoom <- State.inspect((_: Roomba).currentRoom)
            chargingStationRoom <- State.inspect(
              (_: Roomba).chargingStationRoom
            )
            nextState =
              if e == Some(Event.Start) then Cleaning
              else if newRoom == chargingStationRoom then Charging
              else GoingCharging
          yield (nextState)

      for
        _ <- handleChangeModeEvent
        s <- handleActivities
      yield (s)
