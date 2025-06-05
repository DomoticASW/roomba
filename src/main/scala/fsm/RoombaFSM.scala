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
      for
        s <- currentState
        _ <- s match
          case Cleaning =>
            for
              _ <- FSM.setCountdown("battery", batteryRate)
              _ <- FSM.setCountdown("changeRoom", changeRoomRate)
            yield ()
          case GoingCharging => FSM.setCountdown("changeRoom", changeRoomRate)
          case Charging      => FSM.setCountdown("battery", batteryRate)
      yield ()

    override def onActive(
        e: Option[Event],
        timePassed: Long
    ): State[RoombaFSM, RoombaState] =
      for
        _ <- e match
          case Some(Event.ChangeMode(mode)) => setMode(mode)
          case _                            => State.same
        s <- currentState
        nextState <- s match
          case Cleaning =>
            for
              updateBattery <- FSM.resetCountdownIfReached("battery")
              changeRoom <- FSM.resetCountdownIfReached("changeRoom")
              _ <- if updateBattery then decBattery() else State.same
              _ <- if changeRoom then changeRoomRandom() else State.same
              battery <- FSM.inspect((_: Roomba).battery)
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
              newRoom <- FSM.inspect((_: Roomba).currentRoom)
              chargingStationRoom <- FSM.inspect(
                (_: Roomba).chargingStationRoom
              )
              nextState =
                if e == Some(Event.Start) then Cleaning
                else if newRoom == chargingStationRoom then Charging
                else GoingCharging
            yield (nextState)
      yield (nextState)
