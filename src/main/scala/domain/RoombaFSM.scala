package domain

import scala.util.Random
import fsm.FSM.*
import java.util.UUID

private[domain] object RoombaFSM:

  enum State:
    case Cleaning
    case GoingCharging
    case Charging
  import State.*

  enum Mode:
    case Silent
    case Performance
    case DeepCleaning

    override def toString(): String =
      this match
        case Silent       => "Silent"
        case Performance  => "Performance"
        case DeepCleaning => "Deep cleaning"

  object Mode:
    def unapply(s: String): Option[Mode] =
      Mode.values.find(m => m.toString() == s)

  enum Event:
    case ChangeMode(m: Mode)
    case Start
    case Stop
  import Event.*

  case class RoombaData(
      id: String,
      name: String,
      battery: Int,
      mode: Mode,
      currentRoom: String,
      chargingStationRoom: String,
      rooms: Set[String],
      batteryRateMs: Long,
      changeRoomRateMs: Long,
      lanHostname: String
  )

  object Countdowns:
    val battery = "battery"
    val changeRoom = "changeRoom"

  def apply(
      data: RoombaData,
      initialState: State
  ): FSM[State, RoombaData, Event] =
    FSM(initialState, data)

  given FSMState[State, RoombaData, Event] with
    override def onEntry(): FSMState[Unit] =
      matchCurrentState:
        _ match
          case Cleaning =>
            for
              _ <- setBatteryCountdown
              _ <- setChangeRoomCountdown
            yield ()
          case GoingCharging => setChangeRoomCountdown
          case Charging      => setBatteryCountdown

    override def onActive(e: Option[Event], timePassed: Long): FSMState[State] =
      for
        _ <- e match
          case Some(ChangeMode(mode)) => setMode(mode)
          case _                      => same
        nextState <- matchCurrentState:
          _ match
            case Cleaning =>
              for
                _ <- ifCountdownReached(Countdowns.battery)(decBattery())
                _ <- ifCountdownReached(Countdowns.changeRoom)(
                  changeRoomRandom()
                )
                _ <- resetCountdownIfReached(Countdowns.battery)
                _ <- resetCountdownIfReached(Countdowns.changeRoom)
                battery <- inspect(_.battery)
                nextState =
                  if battery <= 10 || e == Some(Stop) then GoingCharging
                  else Cleaning
              yield (nextState)
            case Charging =>
              for
                _ <- ifCountdownReached(Countdowns.battery)(incBattery())
                _ <- resetCountdownIfReached(Countdowns.battery)
                nextState =
                  if e == Some(Start) then Cleaning
                  else Charging
              yield (nextState)
            case GoingCharging =>
              for
                _ <- ifCountdownReached(Countdowns.changeRoom)(
                  changeRoomRandom()
                )
                _ <- resetCountdownIfReached(Countdowns.changeRoom)
                newRoom <- inspect(_.currentRoom)
                chargingRoom <- inspect(_.chargingStationRoom)
                nextState =
                  if e == Some(Start) then Cleaning
                  else if newRoom == chargingRoom then Charging
                  else GoingCharging
              yield (nextState)
      yield (nextState)

    private def setBatteryCountdown: FSMState[Unit] =
      for
        ms <- inspect(_.batteryRateMs)
        _ <- setCountdown(Countdowns.battery, ms)
      yield ()
    private def setChangeRoomCountdown: FSMState[Unit] =
      for
        ms <- inspect(_.changeRoomRateMs)
        _ <- setCountdown(Countdowns.changeRoom, ms)
      yield ()
    private def setMode(mode: Mode): FSMState[Unit] =
      modified(_.copy(mode = mode))
    private def incBattery(): FSMState[Unit] = modified: r =>
      r.battery match
        case b if b < 100 => r.copy(battery = b + 1)
        case _            => r
    private def decBattery(): FSMState[Unit] = modified: r =>
      r.battery match
        case b if b > 0 => r.copy(battery = b - 1)
        case _          => r
    private def changeRoomRandom(): FSMState[Unit] =
      modified(r => r.copy(currentRoom = Random().shuffle(r.rooms).head))
