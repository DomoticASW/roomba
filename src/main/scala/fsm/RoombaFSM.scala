package fsm

import state.State.*

object RoombaFSM:
  import scala.util.Random
  import domain.Roomba.{State as RoombaState, *}
  import RoombaState.*

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

  private def handleBatteryCountdown(ms: Long): State[Model, Boolean] =
    for
      countDown <- State.inspect((_: Model).batteryCountdown)
      reached = countDown <= 0
      _ <-
        if reached then
          State.modify((m: Model) => m.copy(batteryCountdown = m.batteryRate))
        else
          State.modify((m: Model) =>
            m.copy(batteryCountdown = m.batteryCountdown - ms)
          )
    yield (reached)
  private def handleChangeRoomCountdown(ms: Long): State[Model, Boolean] =
    for
      countDown <- State.inspect((_: Model).changeRoomCountdown)
      reached = countDown <= 0
      _ <-
        if reached then
          State.modify((m: Model) =>
            m.copy(changeRoomCountdown = m.changeRoomRate)
          )
        else
          State.modify((m: Model) =>
            m.copy(changeRoomCountdown = m.changeRoomCountdown - ms)
          )
    yield (reached)
  private def setMode(mode: Mode): State[Model, Unit] =
    State.modify((m: Model) => m.copy(r = m.r.update(mode = mode)))
  private def incBattery(): State[Model, Unit] =
    State.modify((m: Model) =>
      m.copy(r = m.r.update(battery = m.r.battery + 1))
    )
  private def decBattery(): State[Model, Unit] =
    State.modify((m: Model) =>
      m.copy(r = m.r.update(battery = m.r.battery - 1))
    )
  private def changeRoomRandom(): State[Model, Unit] =
    State.modify((m: Model) =>
      m.copy(r = m.r.update(currentRoom = Random().shuffle(m.r.rooms).head))
    )

  given States[RoombaState, Model, Event] with
    def onEntry(s: RoombaState): State[Model, Unit] = State.same
    def onExit(s: RoombaState): State[Model, Unit] = State.same
    def onActive(
        s: RoombaState,
        e: Option[Event],
        timePassed: Long
    ): State[Model, RoombaState] =
      val handleChangeModeEvent =
        for _ <- e match
            case Some(Event.ChangeMode(mode)) => setMode(mode)
            case _                            => State.same
        yield ()
      val handleActivities = s match
        case Cleaning =>
          for
            updateBattery <- handleBatteryCountdown(timePassed)
            changeRoom <- handleChangeRoomCountdown(timePassed)
            _ <- if updateBattery then decBattery() else State.same
            _ <- if changeRoom then changeRoomRandom() else State.same
            battery <- State.inspect((_: Model).r.battery)
            nextState =
              if battery <= 10 || e == Some(Event.Stop) then GoingCharging
              else Cleaning
          yield (nextState)
        case Charging =>
          for
            updateBattery <- handleBatteryCountdown(timePassed)
            _ <- if updateBattery then incBattery() else State.same
            nextState =
              if e == Some(Event.Start) then Cleaning
              else Charging
          yield (nextState)
        case GoingCharging =>
          for
            changeRoom <- handleChangeRoomCountdown(timePassed)
            _ <- if changeRoom then changeRoomRandom() else State.same
            newRoom <- State.inspect((_: Model).r.currentRoom)
            chargingStationRoom <- State.inspect(
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
