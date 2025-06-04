package fsm

import State as StateMonad
import State.State as StateMonad

object RoombaFSM:
  import scala.util.Random
  import domain.Roomba
  import domain.Roomba.*
  import domain.Roomba.State.*

  case class Model(
      r: Roomba,
      batteryRate: Long,
      batteryCountdown: Long,
      changeRoomRate: Long,
      changeRoomCountdown: Long
  )

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop

  import FSM.*

  private def handleBatteryCountdown(ms: Long): StateMonad[Model, Boolean] =
    for
      countDown <- StateMonad.inspect((_: Model).batteryCountdown)
      reached = countDown <= 0
      _ <-
        if reached then
          StateMonad.modify((m: Model) =>
            m.copy(batteryCountdown = m.batteryRate)
          )
        else
          StateMonad.modify((m: Model) =>
            m.copy(batteryCountdown = m.batteryCountdown - ms)
          )
    yield (reached)
  private def handleChangeRoomCountdown(ms: Long): StateMonad[Model, Boolean] =
    for
      countDown <- StateMonad.inspect((_: Model).changeRoomCountdown)
      reached = countDown <= 0
      _ <-
        if reached then
          StateMonad.modify((m: Model) =>
            m.copy(changeRoomCountdown = m.changeRoomRate)
          )
        else
          StateMonad.modify((m: Model) =>
            m.copy(changeRoomCountdown = m.changeRoomCountdown - ms)
          )
    yield (reached)
  private def setMode(mode: Mode): StateMonad[Model, Unit] =
    StateMonad.modify((m: Model) => m.copy(r = m.r.update(mode = mode)))
  private def incBattery(): StateMonad[Model, Unit] =
    StateMonad.modify((m: Model) =>
      m.copy(r = m.r.update(battery = m.r.battery + 1))
    )
  private def decBattery(): StateMonad[Model, Unit] =
    StateMonad.modify((m: Model) =>
      m.copy(r = m.r.update(battery = m.r.battery - 1))
    )
  private def changeRoomRandom(): StateMonad[Model, Unit] =
    StateMonad.modify((m: Model) =>
      m.copy(r = m.r.update(currentRoom = Random().shuffle(m.r.rooms).head))
    )

  given States[State, Model, Event] with
    def onEntry(s: State): StateMonad[Model, Unit] = StateMonad.same
    def onExit(s: State): StateMonad[Model, Unit] = StateMonad.same
    def onActive(
        s: State,
        e: Option[Event],
        timePassed: Long
    ): StateMonad.State[Model, State] =
      val handleChangeModeEvent =
        for _ <- e match
            case Some(Event.ChangeMode(mode)) => setMode(mode)
            case _                            => StateMonad.same
        yield ()
      val handleActivities = s match
        case Cleaning =>
          for
            updateBattery <- handleBatteryCountdown(timePassed)
            changeRoom <- handleChangeRoomCountdown(timePassed)
            _ <- if updateBattery then decBattery() else StateMonad.same
            _ <- if changeRoom then changeRoomRandom() else StateMonad.same
            battery <- StateMonad.inspect((_: Model).r.battery)
            nextState =
              if battery <= 10 || e == Some(Event.Stop) then GoingCharging
              else Cleaning
          yield (nextState)
        case Charging =>
          for
            updateBattery <- handleBatteryCountdown(timePassed)
            _ <- if updateBattery then incBattery() else StateMonad.same
            nextState =
              if e == Some(Event.Start) then Cleaning
              else Charging
          yield (nextState)
        case GoingCharging =>
          for
            changeRoom <- handleChangeRoomCountdown(timePassed)
            _ <- if changeRoom then changeRoomRandom() else StateMonad.same
            newRoom <- StateMonad.inspect((_: Model).r.currentRoom)
            chargingStationRoom <- StateMonad.inspect(
              (_: Model).r.chargingStationRoom
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
